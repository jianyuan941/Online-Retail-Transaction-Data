#| include: false
#install.packages("writexl")
packages <- function(  
    ask = FALSE,
    check_built = TRUE){

  install.packages("echarts4r", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("cowplot", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("ggpubr", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("reactable", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("lubridate", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org") #for date 
  install.packages("hrbrthemes", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org") #ggplot theme
  install.packages("scales", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("ggrepel", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("quarto", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("shiny", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("htmltools", ask = ask, checkBuilt = check_built, repos = "https://cloud.r-project.org")
  install.packages("patchwork",repos = "https://cloud.r-project.org")
  install.packages("plotly",repos = "https://cloud.r-project.org")
  
  #install.packages("plotly", type = "binary")
  library("skimr")
  library("tidyverse")
  library("kableExtra")
  library("dplyr")
  library("janitor")
  library("echarts4r")
  #library("writexl")
  library("rlang")
  library("dplyr")
  library("echarts4r")
  library("plotly")
  library("cowplot")
  library("reactable")
  library("lubridate")
  library(ggpubr)
  library(hrbrthemes)
  library("scales")
  library("ggrepel")
  library("shiny")
  library("quarto")
  library("htmltools")
  library("patchwork")
  library("plotly")

}
packages()



#Interesting commant ================================================================================================================
#1. par(mfrow=c(1,2)) this command is to allow 2 graph display in 1 window
#2. Pairs(a,b,c,d,...) this allow multiple scatter plot to display to provide insight of any pair of relationshi
    
#function----------------------------------------------------------------------------------
#To load csv/rds file from folder
read_file <- function(
    data_base = NULL, #need setup file_path before used| eg."test.rds"
    file_path = "C:\\Users\\JY\\Desktop\\R script\\Online-Retail-Transaction-Data\\source data\\"
    ){
  
  if(str_detect(as.character(enquo(data_base)[2]), regex("\\.rds",ignore_case = T)) == T){
  rds_df <- readRDS(file.path("rds file", data_base))
  
    trim <- str_replace_all(as.character(enquo(data_base)[2]),
                            regex("\\.rds",ignore_case = T)
                            ,"")
    
    assign(trim, rds_df, envir = .GlobalEnv)
    
    return(rds_df)
  }else{
  
  full_file_path <- paste0(file_path, data_base) 
  csv_df <- read.csv(full_file_path)
  
      trim <- str_replace_all(as.character(enquo(data_base)[2]),
                              regex("\\.csv",ignore_case = T)
                              ,"")
  
    assign(trim, csv_df, envir = .GlobalEnv)
    
    return(csv_df)
  }
  
}
save_function<-function(
    data_base = NULL,#eg."test.rds"
    name = NULL,
    type = "rds", #can change to "CSV"|"rds"|"NULL"
    return = F  #whether want to continue write after save
){
  
  if(!as.character(enquo(name)[2])== "NULL"){
    
        if(type == "rds"){
          
          if(dir.exists("rds file")){
            file_name <- paste0(as.character(enquo(name)[2]),".rds")
            saveRDS(data_base, file = file.path("rds file",file_name))
          }else{
            dir.create("rds file")
            file_name <- paste0(as.character(enquo(name)[2]),".rds")
            saveRDS(data_base, file = file.path("rds file",file_name))
          }      
          
        }else if(type == "csv"){
          if(dir.exists("csv file")){
              file_name <- paste0(as.character(enquo(name)[2]),".csv")
              write.csv(data_base, file = file.path("csv file",file_name), row.names = FALSE)
            }else{
              dir.create("csv file")
              file_name <- paste0(as.character(enquo(name)[2]),".csv")
              write.csv(data_base, file = file.path("csv file",file_name), row.names = FALSE)
            }
          }else if(type == "NULL"){
            assign(as.character(enquo(name)[2]), data_base, envir = .GlobalEnv)
        }
          
    
  }else{
    
    if(type == "rds"){
      
      if(dir.exists("rds file")){
        file_name <- paste0(as.character(enquo(data_base)[2]),".rds")
        saveRDS(data_base, file = file.path("rds file",file_name))
      }else{
        dir.create("rds file")
        file_name <- paste0(as.character(enquo(data_base)[2]),".rds")
        saveRDS(data_base, file = file.path("rds file",file_name))
      } 
      
    }else{
      
      if(dir.exists("csv file")){
        file_name <- paste0(as.character(enquo(data_base)[2]),".csv")
        write.csv(data_base, file = file.path("csv file",file_name), row.names = FALSE)
      }else{
        dir.create("csv file")
        file_name <- paste0(as.character(enquo(data_base)[2]),".csv")
        write.csv(data_base, file = file.path("csv file",file_name), row.names = FALSE)
      }
    }
    
  }
 
  
  if(return == F){
  }else{
    return(data_base)
  }
}
#to check summarized sales/unit base on primary_id
Group_category_to_summary <- function( data, category_col, summarize_col, new_data_set){
  cat <- enquo(category_col)
  sum <- enquo(summarize_col)
  df<- data %>%
    group_by(!!cat) %>% 
    summarise(total_quantity = sum(!!sum, na.rm = T)) %>% 
    arrange(desc(total_quantity))
  
  #form new dataset
  if(new_data_set !=""){
    assign(new_data_set, df, envir = .GlobalEnv)
  }else{
    "yes"
  }
  
  #dispaly searchable table
  reactable(
    df,
    filterable = TRUE,
    sortable = TRUE,
    pagination = TRUE,
    defaultPageSize = ,
    columns = setNames(
      lapply(names(df), function(col) {
        colDef(
          align = "center",
          style = list(textAlign = "center"),
          headerStyle = list(textAlign = "center")
        )
      }),
      names(df)
    )
  )
  
  
}

#to check for inconsistent description base on primary_id
Check_description_base_on_id <- function(data, id, description,  new_data_set) {
  
  id <- enquo(id)
  descip <- enquo(description)
  
  unique_ids <- data %>% select(!!id) %>% distinct()
  number <- nrow(unique_ids)
  
  col1 <- c()
  col2 <- c()
  
  for (i in 1:number) {
    current_id <- unique_ids[[1]][i]
    
    col1 <- append(col1, current_id)
    
    count_desc <- data %>%
      filter(!!id == current_id) %>%
      select(!!descip) %>%
      distinct() %>%
      nrow()
    
    col2 <- append(col2, count_desc)
  }
  
  df <- data.frame(ID = col1, Description_Count = col2) %>% arrange(desc(Description_Count))
  
  #form new dataset
  if(new_data_set !=""){
    assign(new_data_set, df, envir = .GlobalEnv)
  }else{
    "yes"
  }
  
  #dispaly searchable table
  reactable(
    df,
    filterable = TRUE,
    sortable = TRUE,
    pagination = TRUE,
    defaultPageSize = 5,
    columns = setNames(
      lapply(names(df), function(col) {
        colDef(
          align = "center",
          style = list(textAlign = "center"),
          headerStyle = list(textAlign = "center")
        )
      }),
      names(df)
    )
  )
  
  
}
month_year_range <- function(
    data=NULL, 
    col_name = NULL,
    start_date=NULL, 
    end_date=NULL) {
  
  col_n <- enquo(col_name)
  
  # Ensure inputs are Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Generate sequence from the first of start month to the first of end month
  month_seq <- seq(
    from = as.Date(format(start_date, "%Y-%m-01")),
    to   = as.Date(format(end_date, "%Y-%m-01")),
    by   = "month"
  )
  
  # Return as "month_year" format (e.g., "Jul_2025")
  order_level <- format(month_seq, "%b %Y")
  
  data %>% mutate(
    !!col_n := factor(!!col_n, levels = order_level, order =TRUE)
  )
}

#To change date format
date_format <- function(data, col_name, fmt = NULL) {
  if(is.null(fmt)){
    fmt <- "%m/%d/%Y %H:%M"
  }
  data %>% mutate(
    {{ col_name }} :=
        as.Date(
          as.POSIXct({{ col_name }}, format = fmt)
        )
      )
}

join_word <- function(x){
          new_join <- c()
          new_x <- sort(x)
          for(n in 1:length(unique(new_x))){
            if(n == 1){
              join <- unique(new_x)[n]  
            }
            else if(n == length(unique(new_x))){
              join <- paste0("and ",unique(new_x)[n])
            }else {
              join <- paste0(", ", unique(new_x)[n])
            }
            new_join <- c(new_join, join) %>% paste0(., collapse =" ")
          }
          #Output: 1,2,3,...and n
          return(new_join)
}

link <- function( url, new_tab = FALSE) {
          target <- if (new_tab) " target='_blank'" else ""
          paste0("<a href='", url, "'", target, ">", "Direct", "</a>")
}

sig_multiple <- function (col, single, multiple){
          min_value <- min(unique(nchar(col))) 
          max_value <- max(unique(nchar(col))) 
          word <- if(max_value>1){multiple}else{single}
          combine <- if(max_value>1){
            paste0(min_value, " to ", max_value, word)}else{
            paste0(min_value, word)  
            }
          # Output: 1 to 2 values/ 1 value
          return(combine)
}

result_display <- function(x,cap){
          x %>%
            tibble("Output :" =.) %>% 
            kbl(caption=cap) %>% 
            kable_styling(position="left")
} 

result_displayv2 <- function(
    x, 
    page_size, 
    new_data_set, 
    type = NULL  #type csv/rds to save new file
    ){
  df <- x
  
  #form new dataset
  if(new_data_set !=""){
    if(as.character(enquo(type)[2]) == "NULL"){
      
      assign(new_data_set, df, envir = .GlobalEnv)
      
    } else if(as.character(enquo(type)[2]) == "rds"){
      
      save_function(
        data_base = df,
        name = included_new_db,
        type ="rds",
        return = F
      )
      
    } else if(as.character(enquo(type)[2]) == "csv"){
      
      save_function(
        data_base = df,
        name = included_new_db,
        type ="csv",
        return = F
      )
      
    }
  }else{
  }
  
  reactable(
    x,
    filterable = TRUE,
    sortable = TRUE,
    pagination = TRUE,
    defaultPageSize = page_size,
    columns = setNames(
      lapply(seq_along(names(df)), function(i) {
        col <- names(df)[i]
        colDef(
          sticky = if (i == 1) "left" else NULL,
          align = "center",
          header = tags$div(title = col, col),
          style = list(textAlign = "center", 
                       borderRight = if (i == 1) "2px solid #ccc" else NULL),
          headerStyle = list(textAlign = "center", 
                             borderRight = if (i == 1) "2px solid #ccc" else NULL)
        )
      }),
      names(x)
    )
  )
  
}

h4_header <- function(id,title) {
          paste0(
            "<hr>\n",
            "<h4 id='", id, "'>", title, "</h4>\n"
          )
}

combined_function <- function(file_name, dataframe_name, folder_path){
  
          file_path <- paste0(folder_path, file_name)
          
          #adjust column name
          #trim data
          
          df <- read.csv(file_path) %>% 
            clean_names() %>% 
            mutate(passenger_id = as.character(passenger_id))
          df[] <- lapply(df, function(col){
            if(is.character(col)) trimws(col) 
            else (col)
          })
          
          assign(dataframe_name, df, envir = .GlobalEnv)
          
          #return as skim for futher checking
          return(skim(df))
}
merged <- function(
  dataset1 = NULL,
  dataset2 = NULL,
  col_name = NULL,
  type_of_join = NULL #left/right/all/inner
  ){
    col <- as.character(enquo(col_name)[2])
    merge(dataset1, dataset2, by = col, all.x =T)
  }

#Form new dataset for analysis purpose
  new_data <- function(n){
    df <- data.frame(
      ticket = merged_data$ticket,
      age_group = merged_data$age_group,
      survived = merged_data$survived,
      fare = merged_data$fare, 
      sib_sp = merged_data$sib_sp, 
      parch = merged_data$parch,
      embarked = merged_data$embarked,
      pclass = merged_data$pclass)
    
    assign(n, df, envir = .GlobalEnv)
    
    #Find any fare column with NA value before remove
    df[which(rowSums(is.na(df)) > 0),] %>% 
      kbl() %>% 
      kable_styling()
    
  }
#=====================================================
#MERGED PLOT
  merged_plot<- function(graph1, graph2){
    plot_grid(
      NULL,graph1, graph2, NULL,
      ncol = 4,
      rel_widths = c(0.1, 1, 1, 0.1)
    )
  }
#MERGED PLOT FULL
  merged_plot_full <-function(graph1, graph2,graph_y_axis,tit, title_y_axis){ #default graph_y_axis = -0.1, title_y_axis = 0.8
    ggdraw() +
      draw_label(tit, 
                 fontface = 'bold', 
                 x = 0.5, 
                 y = title_y_axis, 
                 hjust = 0.5, size = 16) +
      draw_plot(
        merged_plot(
          graph1,graph2), 
        x = 0, 
        y = graph_y_axis, 
        width = 1, 
        height = 0.95)
  }
#====================================================
