des_analysis <- function(
    data_base = NULL,
    sample = TRUE,
    x = NULL,
    f = NULL, #optional
    accept_negative_x = F, #optional
    included_new_db = NULL, #optional
    excluded_new_db = NULL, #optional
    new_db_primary_key = NULL #optional
){
  
  n <- enquo(x)
  fr <- enquo(f)
  primary_key <- enquo(new_db_primary_key)
  
  x_col <- substitute(x) # for transfer parameter purpose
  f <- substitute(f) # for transfer parameter purpose
  
  cols_to_select <- 
    if(as.character(primary_key[2]) == "NULL" && as.character(fr[2]) != "NULL"){
      c(rlang::as_name(n), rlang::as_name(fr))
    } else if(as.character(primary_key[2]) == "NULL" && as.character(fr[2]) == "NULL"){
      c(rlang::as_name(n))
    } else if(as.character(primary_key[2]) != "NULL" && as.character(fr[2]) != "NULL"){  
      c(rlang::as_name(primary_key), rlang::as_name(n), rlang::as_name(fr))
    } else if(as.character(primary_key[2]) != "NULL" && as.character(fr[2]) == "NULL"){ 
      c(rlang::as_name(primary_key), rlang::as_name(n))
    }
  
  ##prepare database for calculation
  if(!is.null(f)){
    df <- data_base %>%
      filter(
        #Pre-condition whether -x is acceptable
        if(accept_negative_x){
          !is.na(!!n)&!!fr>0 
        }else{
          !is.na(!!n)&!!n>=0&!!fr>0 
        }
      ) %>%
      mutate(!!fr := ifelse(is.na(!!fr), 0, !!fr)) %>%
      select(all_of(cols_to_select))    
  }else{
    df <- data_base %>%
      filter(
        #Pre-condition whether -x is acceptable
        if(accept_negative_x){
          !is.na(!!n) 
        }else{
          !is.na(!!n)&!!n>=0 
        }
      ) %>%
      select(all_of(cols_to_select))          
  }
  
    ###Assigning db for dataframe
    if(!is.null(included_new_db)){
      assign(included_new_db, df, envir = .GlobalEnv)
    }
    if(!is.null(excluded_new_db)){
      
      if(!is.null(f)){
        excluded_df <- data_base %>%
          filter(
            #if accept_negative mean not acceptance:
            #1. dont want include x <0
            #2. include x = na
            
            if(accept_negative_x){
               is.na(!!n)|!!fr<=0 #(x = na, freq <0)
            }else{
              is.na(!!n)|n<0|fr<=0#(x is na, n<0 ,frequency<=0)
            }
          ) %>%
          mutate(!!fr := ifelse(is.na(!!fr), 0, !!fr)) %>%
          select(all_of(cols_to_select))    
      }else{
        excluded_df <- data_base %>%
          filter(
            #Pre-condition whether -x is acceptable
            if(accept_negative_x){
              is.na(!!b) #(n is na)
            }else{
              is.na(!!n)|!!n<0 #(n is na, n<0)
            }
          ) %>%
          select(all_of(cols_to_select))          
      }
      
      assign(excluded_new_db, exclude_df, envir = .GlobalEnv)
    }
  
  ####Descriptive Analysis================================================================================
  mean_result <- mean_function(db = df,internal_x = x_col,internal_f = f) 
  Q1_result <- Q1_function(db = df,internal_x = x_col,internal_f = f)
  median_result <- median_function(db = df,internal_x = x_col,internal_f = f)
  Q3_result <- Q3_function(db = df,internal_x = x_col,internal_f = f)
  mode_result <- mode_function(db = df,internal_x = x_col,internal_f = f)
  
  Des1 <- if(median_result>mean_result){"(negative skewness)"} 
  else if(median_result<mean_result){"(positve skewness)"}
  else if(mean_result == median_result){"(no skew)"}
  
  ####Spreading Calculation================================================================================
  variance_result <- variance_function(db = df, internal_x = x_col, sample = sample, mean =mean_result)
  sd_result <- sqrt(variance_result)
  
  #Parse data for continue working
  x_col <- deparse(x_col)
  f_col <- deparse(f)
  
  mx <- max(df[[x_col]])
  mn <- min(df[[x_col]])
  
  range <- mx - mn
  IQR <- Q3_result - Q1_result
  
  ####2nd Layer Calculation================================================================================
  
  #Formula for outliner
  Upper_bound_base_IQR <- Q3_result + 1.5*IQR
  Lower_bound_base_IQR <- Q1_result - 1.5*IQR
  Upper_bound_base_Mean <- mean_result + 2*sd_result
  Lower_bound_base_Mean <- mean_result - 2*sd_result
  
  #Calculate Outliner
  Upper_outliner <- sum(df[[x_col]] >Upper_bound_base_IQR)
  Lower_outliner <- sum(df[[x_col]] <Lower_bound_base_IQR)
  
  #Calculate within Boundary
  if(!is.null(f_col) && (f_col) == "NULL"){
    higher_boundary <- sum(df[[x_col]] <= Upper_bound_base_IQR) - sum(df[[x_col]] < Q3_result)
    within_boundary <- sum(df[[x_col]] <= Q3_result) - sum(df[[x_col]] < Q1_result)
    lower_boundary <- sum(df[[x_col]] < Q1_result) - sum(df[[x_col]] < Lower_bound_base_IQR) 
  }else{
    higher_boundary <- sum(df[[f_col]][df[[x_col]] <= Upper_bound_base_IQR]) - sum(df[[f_col]][df[[x_col]] < Q3_result])
    within_boundary <- sum(df[[f_col]][df[[x_col]] <= Q3_result]) - sum(df[[f_col]][df[[x_col]] < Q1_result])
    lower_boundary <- sum(df[[f_col]][df[[x_col]] < Q1_result]) - sum(df[[f_col]][df[[x_col]] < Lower_bound_base_IQR]) 
    
  }
  
  Part1 <- paste0("Mean = ", scales :: comma(mean_result, accuracy = 0.01), " ; Median = ",scales :: comma(median_result, accuracy = 0.01), " ; Mode = ",scales :: comma(mode_result, accuracy = 0.01), " ; ", Des1)
  Part2 <- if(sample){
    paste0("S² = ", scales :: comma(variance_result, accuracy = 0.01)," ; S = ", scales :: comma(sd_result, accuracy = 0.01))
  }else{
    paste0("σ² = ", scales :: comma(variance_result, accuracy = 0.01)," ; σ = ", scales :: comma(sd_result, accuracy = 0.01))
  }
  Part3 <- paste0("Range = ",scales :: comma(mn, accuracy = 0.01), ": ", scales :: comma(mx, accuracy = 0.01)," (", scales :: comma(range, accuracy = 0.01),")")
  Part4 <- paste0("Q1 = ", scales :: comma(Q1_result, accuracy = 0.01)," ; Q3 = ", scales :: comma(Q3_result, accuracy = 0.01)," (",scales :: comma(IQR, accuracy = 0.01),")")
  Part5 <- paste0("Lower = ",scales :: comma(Lower_bound_base_IQR, accuracy = 0.01)," ; Upper = ",scales :: comma(Upper_bound_base_IQR, accuracy = 0.01))
  Part6 <- paste0("Upper = ",scales :: comma(Upper_outliner, accuracy = 0.01))
  Part7 <- paste0("2nd-Upper = ", scales :: comma(higher_boundary, accuracy = 0.01))
  Part8 <- paste0("Middle = ",scales :: comma(within_boundary, accuracy = 0.01))
  Part9 <- paste0("2nd-Bottom = ", scales :: comma(lower_boundary, accuracy = 0.01))
  Part10 <- paste0("Lower = ", scales :: comma(Lower_outliner, accuracy = 0.01))
  
  results <- c("Central Tendency:",
               Part1,
               "Variability:",
               Part2,
               Part3,
               Part4,
               "Boundary:",
               Part5,
               "Distribution: ", 
               Part6,
               Part7,
               Part8,
               Part9,
               Part10
  )
  results %>% result_display(.,"")
}
### Sub Function of des_analysis============================================================================


                    mode_function <- function(
                        db = NULL,
                        internal_x = NULL, #this for internal x
                        internal_f = NULL, #this for internal frequency
                        x = NULL,
                        f = NULL){
                      
                      sub_f <- substitute(f)
                      
                      #Deparse X
                      if(!is.null(internal_x)){
                        x_col <- deparse(internal_x) #deparse only when data from internal calculation
                      }else{
                        x_col <- deparse(substitute(x)) #deparse + sub if data from direct calculation
                      }
                      #Deparse F
                      if(is.null(internal_f) && is.null(sub_f)){ #do nothing
                        #return("nothing")
                      }else if(!is.null(internal_f) && nchar(internal_f)>0){
                        f_col <- deparse(internal_f)  #deparse only when data from internal calculation
                        #return("internal_f")
                      }else if(!is.null(sub_f) && nchar(sub_f) >0){
                        #return("sub_f")
                        f_col <- deparse(substitute(f)) #deparse + sub if data from direct calculation
                      }
                      
                      #Condition checking whether freq is present
                      if(!is.null(f_col) && nchar(f_col)>1){
                        #Mean calculation WITH frequency
                        expanded_values <- rep(db[[x_col]], floor(db[[f_col]]))
                        freq_table <- table(expanded_values)
                        mode_values <- names(freq_table)[freq_table == max(freq_table)]
                        as.numeric(mode_values)
                      }else{
                        #Mode calculation WITHOUT frequency
                        unique_x <- unique(db[[x_col]])
                        round(
                          unique_x[which.max(tabulate(match(db[[x_col]], unique_x)))]
                          ,2)
                      }
                      
                    }
                    
                    median_function <- function(
                        db = NULL,
                        internal_x = NULL, #this for internal x
                        internal_f = NULL, #this for internal frequency
                        x = NULL,
                        f = NULL){
                      
                      sub_f <- substitute(f)
                      
                      #Deparse X
                      if(!is.null(internal_x)){
                        x_col <- deparse(internal_x) #deparse only when data from internal calculation
                      }else{
                        x_col <- deparse(substitute(x)) #deparse + sub if data from direct calculation
                      }
                      #Deparse F
                      if(is.null(internal_f) && is.null(sub_f)){ #do nothing
                        #return("nothing")
                      }else if(!is.null(internal_f) && nchar(internal_f)>0){
                        f_col <- deparse(internal_f)  #deparse only when data from internal calculation
                        #return("internal_f")
                      }else if(!is.null(sub_f) && nchar(sub_f) >0){
                        #return("sub_f")
                        f_col <- deparse(substitute(f)) #deparse + sub if data from direct calculation
                      }
                      
                      #Condition checking whether freq is present
                      if(!is.null(f_col) && nchar(f_col)>1){
                        #Median calculation WITH frequency
                        round(
                          quantile(rep(db[[x_col]],db[[f_col]]), probs = 0.5, na.rm = T)
                          ,2)
                      }else{
                        #Median calculation WITHOUT frequency
                        round(
                          median(db[[x_col]], na.rm = T)
                          ,2)
                      }
                      
                    }
                    Q1_function <- function(
                        db = NULL,
                        internal_x = NULL, #this for internal x
                        internal_f = NULL, #this for internal frequency
                        x = NULL,
                        f = NULL){
                      
                      sub_f <- substitute(f)
                      
                      #Deparse X
                      if(!is.null(internal_x)){
                        x_col <- deparse(internal_x) #deparse only when data from internal calculation
                      }else{
                        x_col <- deparse(substitute(x)) #deparse + sub if data from direct calculation
                      }
                      #Deparse F
                      if(is.null(internal_f) && is.null(sub_f)){ #do nothing
                        #return("nothing")
                      }else if(!is.null(internal_f) && nchar(internal_f)>0){
                        f_col <- deparse(internal_f)  #deparse only when data from internal calculation
                        #return("internal_f")
                      }else if(!is.null(sub_f) && nchar(sub_f) >0){
                        #return("sub_f")
                        f_col <- deparse(substitute(f)) #deparse + sub if data from direct calculation
                      }
                      
                      #Condition checking whether freq is present
                      if(!is.null(f_col) && nchar(f_col)>1){
                        #Median calculation WITH frequency
                        round(
                          quantile(rep(db[[x_col]],db[[f_col]]), probs = 0.25, na.rm = T)
                          ,2)
                      }else{
                        #Median calculation WITHOUT frequency
                        round(
                          quantile(db[[x_col]], probs =0.25, na.rm =T)
                          ,2)
                      }
                      
                    }
                    Q3_function <- function(
                        db = NULL,
                        internal_x = NULL, #this for internal x
                        internal_f = NULL, #this for internal frequency
                        x = NULL,
                        f = NULL){
                      
                      sub_f <- substitute(f)
                      
                      #Deparse X
                      if(!is.null(internal_x)){
                        x_col <- deparse(internal_x) #deparse only when data from internal calculation
                      }else{
                        x_col <- deparse(substitute(x)) #deparse + sub if data from direct calculation
                      }
                      #Deparse F
                      if(is.null(internal_f) && is.null(sub_f)){ #do nothing
                        #return("nothing")
                      }else if(!is.null(internal_f) && nchar(internal_f)>0){
                        f_col <- deparse(internal_f)  #deparse only when data from internal calculation
                        #return("internal_f")
                      }else if(!is.null(sub_f) && nchar(sub_f) >0){
                        #return("sub_f")
                        f_col <- deparse(substitute(f)) #deparse + sub if data from direct calculation
                      }
                      
                      #Condition checking whether freq is present
                      if(!is.null(f_col) && nchar(f_col)>1){
                        #Median calculation WITH frequency
                        round(
                          quantile(rep(db[[x_col]],db[[f_col]]), probs = 0.75, na.rm = T)
                          ,2)
                      }else{
                        #Median calculation WITHOUT frequency
                        round(
                          quantile(db[[x_col]], probs =0.75, na.rm =T)
                          ,2)
                      }
                      
                    }
                    mean_function<- function(
                        db = NULL,
                        internal_x = NULL, #this for internal x
                        internal_f = NULL, #this for internal frequency
                        x = NULL,
                        f = NULL){
                      
                      sub_f<- substitute(f)
                      
                      #Deparse X
                      if(!is.null(internal_x)){
                        x_col <- deparse(internal_x) #deparse only when data from internal calculation
                      }else{
                        x_col <- deparse(substitute(x)) #deparse + sub if data from direct calculation
                      }
                      #Deparse F
                      if(is.null(internal_f) && is.null(sub_f)){ #do nothing
                        #return("nothing")
                      }else if(!is.null(internal_f) && nchar(internal_f)>0){
                        f_col <- deparse(internal_f)  #deparse only when data from internal calculation
                        #return("internal_f")
                      }else if(!is.null(sub_f) && nchar(sub_f) >0){
                        #return("sub_f")
                        f_col <- deparse(substitute(f)) #deparse + sub if data from direct calculation
                      }      
                      
                      ##Conditional checking whether freq is present
                      if(!is.null(f_col) && nchar(f_col)>1){
                        #Mean calculation WITH frequency
                        round(
                          weighted.mean(db[[x_col]],db[[f_col]], na.rm = T)
                          ,2)
                      }else{
                        #Mean calculation WITHOUT frequency
                        round(
                          mean(db[[x_col]], na.rm = T)
                          ,2)
                      }
                      
                    }
                    variance_function <- function(
                        db = NULL,
                        sample = TRUE,
                        mean = NULL,
                        x = NULL,
                        internal_x = NULL #this for internal x
                    ){
                      
                      #Deparse X
                      if(!is.null(internal_x)){
                        x_col <- deparse(internal_x) #deparse only when data from internal calculation
                      }else{
                        x_col <- deparse(substitute(x)) #deparse + sub if data from direct calculation
                      }
                      
                      if(!is.null(sample) && sample == T){
                        sum((db[[x_col]]-mean)^2)/(length(db[[x_col]])-1)
                      }else if(!is.null(sample) && sample == F){
                        sum((db[[x_col]]-mean)^2)/length(db[[x_col]])
                      }
                    }
                    
                    