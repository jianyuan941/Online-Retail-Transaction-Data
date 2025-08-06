des_analysis_dataframe_custom <-function(
    data_base = NULL,
    x = NULL,
    accept_negative_x = T, #optional
    f = NULL,
    accept_negative_f = T, #optional, only applicable if x = null
    group = NULL,
    included_new_db = NULL, #optional
    NA_new_db = NULL, #optional
    cross_qmd = FALSE,
    page = NULL, #this is to set nrow in result_display2
    checking = FALSE #this is to uncover the route of calculation
){
  
  #Define
  x_col <- enquo(x); f_col <- enquo(f); group_col <- enquo(group)
  
  
  #Consideration in this part:
  #. always drop_na initially to ensure clean calculation in the  
  #   following stage
  if(as.character(enquo(group)[2]) != "NULL"){ #if group is available
    
    if(checking == T){
      front <- "opening - 1/"
    }
    
    df<- data_base %>% 
      group_by(!!group_col,StockCode)
    
    
    if(as.character(enquo(x)[2]) != "NULL"){
      
      if(checking == T){
        calculation <- "calculation - 1/"
      }
      
      df<-df %>%
        filter(
          #Pre-condition whether -x is acceptable
          if(accept_negative_x == T){
            !!f_col >0
          }else{
            !!x_col >0&!!f_col>0
          }) %>%
        select(StockCode,!!group_col,!!x_col,!!f_col,InvoiceNo, Description) %>% 
        drop_na()%>%
        mutate(
          '∑x' = sum(!!x_col*!!f_col),
          '∑n'= sum(!!f_col),
          #calculation of Mean(total/n)
          mean = sum(!!x_col*!!f_col)/sum(!!f_col),
          median = quantile(rep(!!x_col,!!f_col), probs = 0.5),
          var = round(sum(!!f_col*(!!x_col - mean)^2)/(sum(!!f_col)-1),2),
          sd = sqrt(var),
          'x(68%)' = sum(ifelse(!!x_col<=(mean+sd),!!f_col,0)) - sum(
            ifelse(!!x_col<(mean-sd),!!f_col,0)),
          'x(95%)' = sum(ifelse(!!x_col<=(mean+(2*sd)),!!f_col,0))-
            sum(ifelse(!!x_col<(mean-(2*sd)),!!f_col,0)),
          'x(99%)' = sum(ifelse(!!x_col<=(mean+(3*sd)),!!f_col,0))-
            sum(ifelse(!!x_col<=(mean-(3*sd)),!!f_col,0)),
          '99%+' = sum(ifelse(!!x_col>(mean+(3*sd)),!!f_col,0)) +
            sum(ifelse(!!x_col<(mean-(3*sd)),!!f_col,0))
        ) %>% 
        ungroup()
      
    }else if(as.character(enquo(x)[2]) == "NULL"){
      
      if(checking == T){
        calculation <- "calculation - 2"
      }
      
      df<-df %>%
        filter(
          #Pre-condition whether -x is acceptable
          if(accept_negative_f == T){
            !is.na(!!f_col)
          }else{
            !!f_col>0
          }) %>% 
        select(!!f_col,!!group_col) %>% 
        mutate(
          '∑x' = sum(!!f_col),
          '∑n' = n(),
          #calculation of Mean (total/n)
          mean = sum(!!f_col)/n(),
          median = quantile(!!f_col, probs = 0.5),
          var = sum((!!f_col - (sum(!!f_col)/n()))^2)/(n()-1),
          sd = sqrt(var),
          'x(68%)' = sum(!!f_col<=(mean+sd))-sum(!!f_col<(mean-sd)),
          'x(95%)' = sum(!!f_col<=(mean+(2*sd)))-sum(!!f_col<(mean-(2*sd))),
          'x(99%)' = sum(!!f_col<=(mean+(3*sd)))-sum(!!f_col<(mean-(3*sd))),
          '99%+' = sum(!!f_col>(mean+(3*sd)))+sum(!!f_col<(mean-(3*sd)))
        ) %>% 
        ungroup()
    }
    
  }else if(as.character(enquo(group)[2]) == "NULL"){
    
    if(checking == T){
      front <- "opening - 2"
    }  
    
    df<- data_base %>% 
      drop_na()
    
    if(as.character(enquo(x)[2]) != "NULL"){
      
      if(checking == T){
        calculation <- "calculation - 3"
      }
      
      df<-df %>%
        filter(
          #Pre-condition whether -x is acceptable
          if(accept_negative_x == T){ 
            !!f_col >0
          }else{
            !!x_col >0&!!f_col>0
          }) %>% 
        select(!!x_col,!!f_col) %>%       
        mutate(
          '∑x' = sum(!!x_col*!!f_col),
          '∑n'= sum(!!f_col),
          #calculation of Mean(total/n)
          mean = sum(!!x_col*!!f_col)/sum(!!f_col),
          var = round(sum(!!f_col*(!!x_col - mean)^2)/(sum(!!f_col)-1),2),
          sd = sqrt(var),
          'x(68%)' = sum(ifelse(!!x_col<=(mean+sd),!!f_col,0)) - sum(
            ifelse(!!x_col<(mean-sd),!!f_col,0)),
          'x(95%)' = sum(ifelse(!!x_col<=(mean+(2*sd)),!!f_col,0))-
            sum(ifelse(!!x_col<(mean-(2*sd)),!!f_col,0)),
          'x(99%)' = sum(ifelse(!!x_col<=(mean+(3*sd)),!!f_col,0))-
            sum(ifelse(!!x_col<=(mean-(3*sd)),!!f_col,0)),
          '99%+' = sum(ifelse(!!x_col>(mean+(3*sd)),!!f_col,0)) +
            sum(ifelse(!!x_col<(mean-(3*sd)),!!f_col,0))
        )
      
    }else if(as.character(enquo(x)[2]) == "NULL"){
      
      if(checking == T){
        calculation <- "calculation - 4"
      } 
      df<-df %>%
        filter(
          #Pre-condition whether -x is acceptable
          if(accept_negative_f == T){
            #do nothing
            !is.na(!!f_col)
          }else{
            !!f_col>0
          }) %>% 
        select(!!f_col) %>% 
        mutate(
          '∑x' = sum(!!f_col),
          '∑n' = n(),
          #calculation of Mean (total/n)
          mean = sum(!!f_col)/n(),
          var = sum((!!f_col - (sum(!!f_col)/n()))^2)/(n()-1),
          sd = sqrt(var),
          'x(68%)' = sum(!!f_col<=(mean+sd))-sum(!!f_col<(mean-sd)),
          'x(95%)' = sum(!!f_col<=(mean+(2*sd)))-sum(!!f_col<(mean-(2*sd))),
          'x(99%)' = sum(!!f_col<=(mean+(3*sd)))-sum(!!f_col<(mean-(3*sd))),
          '99%+' = sum(!!f_col>(mean+(3*sd)))+sum(!!f_col<(mean-(3*sd)))
        ) 
    }
  }
  
  if(as.character(enquo(included_new_db)[2])!="NULL"){
    if(cross_qmd == T){
      file_name <- paste0(included_new_db,".rds")
      saveRDS(df, file = file_name)
    }else{
      assign(included_new_db, df, envir = .GlobalEnv)
    }
  }
  if(as.character(enquo(NA_new_db)[2]) != "NULL"){
    exclde <- data_base %>% 
      filter(if_any(everything(),is.na))
    assign(NA_new_db, exclude, envir = .GlobalEnv)
  }
  
  #return calculation route
  if(checking == T){
    return(
      paste0(
        "Utilise Route ",
        front,
        " > ",
        calculation
      )
    )
  }
  #display result
  if(as.character(enquo(page)[2])== "NULL"){
    df %>% result_displayv2(.,5,"")
  }else(
    df %>% result_displayv2(.,page,"")
  )
}
