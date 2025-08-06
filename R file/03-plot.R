#Custom color
custom_green <- "#A8DCAB"


# RANKING LARGE TO SMALL
ranking_large_small <- function(data,column,tit){
  col <- enquo(column)
  data %>% 
    count(!!col, name = "temporary_n") %>% 
    arrange(desc(temporary_n)) %>% 
    ggplot(aes(
      x=fct_reorder(!!col,temporary_n, .desc=FALSE),
      y=temporary_n,
      fill=!!col,
      label=temporary_n,
    ))+
    geom_col()+
    coord_flip()+
    theme_bw()+
    labs(x=NULL,
         y=NULL,
         title=tit)
  #      theme(legend.position ="none",
  #            plot.title = element_text(hjust=-0.2,
  #                                      margin=margin(b=30)
  #            ))+
  #      geom_text(size = 3, #need to have label only can use geom_text
  #                hjust = 1.1,
  #                vjust = 0.25,
  #                col = "black")     
}
# RANKING SMALL TO LARGE
ranking_small_large <- function(data,column,tit){
  col <- enquo(column)
  data %>% 
    count(!!col, name = "temporary_n") %>% 
    arrange(desc(temporary_n)) %>% 
    ggplot(aes(
      x=fct_reorder(!!col,temporary_n, .desc=TRUE),
      y=temporary_n,
      fill=!!col,
      label=temporary_n,
    ))+
    geom_col()+
    coord_flip()+
    labs(x=NULL,
         y=NULL,
         title=tit)+
    theme(legend.position ="none",
          plot.title = element_text(hjust=-0.2,
                                    margin=margin(b=30)
          ))+
    geom_text(size = 3, #need to have label only can use geom_text
              hjust = 1.1,
              vjust = 0.25,
              col = "black")     
}
#BAR CHART
barchart_echart <- function(data, col_for_bar, col_for_xaxis, title) {
  x <- enquo(col_for_bar)
  y <- enquo(col_for_xaxis)
  
  data %>%
    count(!!x, !!y) %>%
    mutate(!!quo_name(y) := as.character(!!y)) %>%     # 🔹 Ensure numeric x-axis
    arrange(!!y) %>%                                 # 🔹 Sort in numeric order
    group_by(!!x) %>%
    e_charts_(x = quo_name(y)) %>%
    e_bar(n, stack = FALSE) %>%
    e_legend() %>% 
    e_title(title) %>% 
    e_legend(orient = "vertical", left = "right", top = "center") %>% #side table
    e_grid(right = 150, bottom = 30)  # Increase this value to add more space
  
  #e_tooltip(trigger = "axis")
}
#BAR CHART - GGPLOT (SINGLE)
barchart_gg <- function(data, column, name_for_title){
  x <- enquo(column)
  
  data %>%
    count(!!x, name = "temporary_n") %>%
    mutate(total = sum(temporary_n)) %>% 
    ggplot(aes(x = !!x,
               y= temporary_n,
               fill = !!x)) +
    
    #1st Layer: Apply type of graph
    geom_col()
  
}

  #BAR CHART - GGPLOT (SINGLE) V2
  barchart_ggv2 <- function(
    data = NULL,
    x_col = NULL,
    y_col = NULL,
    
    #---------------
    stack = T,
    #---------------
    fill_color = NULL, #optional; color base on category               
    bar_color = NULL #optional
    ){
    
    x <- enquo(x_col)
    y <- enquo(y_col)
    fill <- enquo(fill_color)
    
    position_setting <- if(stack){"stack"}else{"dodge"}
    bar_color <- if(as.character(enquo(bar_color)[2]) == "NULL"){"grey"}else{bar_color}
     
    if(as.character(enquo(fill_color)[2]) == "NULL"){
  
      data %>% 
        ggplot(
          aes(
            x = !!x,
            y = !!y
          ))+
          geom_col(fill = bar_color, position = position_setting)+
          theme_ipsum(base_family = "")
        
            
    }else{

      data %>% 
        ggplot(
          aes(
            x = !!x,
            y = !!y,
            fill = !!fill
          )
        )+
        geom_col(position = position_setting)+
        theme_ipsum(base_family = "")
      
    } 
  }
  
  
#BAR CHART - GGPLOT 3 LAYER
barchart_gg_custom <- function(data, x_axis, bar_count, group){
  x <- enquo(x_axis)
  y <- enquo(bar_count)
  fill <- enquo(group)
  
  ggplot(data, aes(x = !!x,
                   y = !!y,
                   fill = !!fill,
                   label = !!y
  ))+
    geom_col()
  
  #layer 1: Apply type of graph => position dodge mean seperate bar base on fill      
  #geom_col(position = position_dodge(width = 1)) +
  
  #layer 2: Apply labels to graph => position dodge mean apply label base on fill;
  #                                  vjust is to adjust height of label        
  #geom_text(position = position_dodge(width = 1), vjust = -0.25, size = 3)
  
  #this is to adjust the y-axis to start with coordination (0,0)
  #scale_y_continuous(expand = c(0,0),
  #                   limits = c(0,120)) #this to set from y = 0 to y = 120
}  

#BOX PLOT 
boxplot_gg <- function(data, 
                       y_axis,
                       ##outliner
                       upper_outliner = NULL, 
                       bottom_outliner = NULL,
                       ##boxplot color
                       box_color = NULL,
                       ##pointer color
                       pointer_color = NULL,
                       outliner_color = NULL
){
  
  ##Default setting for boxplot color
  if(is.null(box_color)){
    box_color = "#69b3a2"
  }
  if(is.null(pointer_color)){
    pointer_color = "#1f77b4"
  }
  if(is.null(outliner_color)){
    outliner_color = "red"
  }
  
  y<- enquo(y_axis)
  
  
  #Calculation logic
  data %>% 
    mutate(
      outliner_checker = !!y < bottom_outliner| !!y >upper_outliner
    ) %>% 
    ggplot(
      aes(
        x = "",
        y = !!y
      )
    )+
    geom_boxplot(
      outlier.shape = NA,
      fill = box_color,
      alpha = 0.4
    )+
    geom_jitter(
      aes(color = outliner_checker),
      width =0.1,
      alpha =0.7,
      size =2
    )+
    theme_ipsum(base_family = "") +
    scale_color_manual(
      values = c("FALSE" = pointer_color ,
                 "TRUE" = outliner_color),
      name = "outliner"
    )
}
#LINE CHART
linechart_gg <- function(data, x, y, point_color = NULL){
  x_axis <- enquo(x)
  y_axis <- enquo(y)
  
  if(is.null(point_color)){
    point_color <- "#1f77b4"
  }
  
  data %>% ggplot(
    aes(
      x = !!x_axis,
      y = !!y_axis
    )
  )+
    theme_bw()+
    geom_line(group = 1)+
    geom_point(color = point_color)+
    theme_ipsum(base_family = "")
  
}

#PIE CHART
piechart_echart <- function(dataset, col,tit){
  x <- enquo(col)
  x_name <- as_name(x)
  
  dataset %>% 
    count(!!x, name = "count") %>% 
    e_charts_(x = x_name) %>% 
    e_title(tit) %>% 
    e_pie_(
      serie = "count",
      label = list(
        formatter = "{b}: {d}%"  # 👈 b = label, d = percent, c = raw count
      )
    ) %>% 
    e_legend(orient = "vertical", left = "right", top = "top") %>% #side table
    e_grid(right = 150, bottom = 30)  # Increase this value to add more space
}
#PIE CHART -GGPLOT (SINGLE ) => combine data manipulation + graphing
piechart_gg <- function(data, column, name_for_legend, name_for_title){
  col <- enquo(column)
  
  data %>% 
    count(!!col, name = "temporary_n") %>% 
    mutate(
      temporary_n_perc = round(temporary_n / sum(temporary_n) * 100, 2),
      label = paste0(temporary_n," (",temporary_n_perc, "%)")
    ) %>% 
    ggplot(aes(x = "", 
               y = temporary_n, 
               fill = !!col)) +
    
    # 1st layer
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    
    # 2nd layer
    labs(fill = name_for_legend, title = name_for_title) +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), 
              size = 4) +
    
    # 3rd layer
    theme_void() +
    theme(legend.position = "right")
}
#PIE CHART -GGPLOTV2 (SINGLE)  => only graphing
piechart_ggv2 <- function(data, column, col_count, label, leg_tit, tit){
  
  ggplot(data, aes(x = "", 
                   y = {{col_count}}, 
                   fill = {{column}})) +
    
    #apply 1st layer: type of graph
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    
    #apply 2nd layer: labs
    labs(fill = leg_tit , title = tit) +
    geom_text(aes(label = {{label}}), position = position_stack(vjust = 0.5), size = 4) +
    
    #apply 3rd layer: adjustment for label, title, legend
    theme_void() +
    theme(legend.position = "right")
}



#TREE MAP
treemap_echart <- function(data, column, tit) {
  df <- data %>%
    count({{ column }}, name = "value") %>%
    mutate(
      name = as.character({{ column }}),
      parent = NA_character_
    ) %>%
    select(name, parent, value)
  
  df %>%
    e_charts() %>%
    e_treemap_(name = "name", value = "value", parents = "parent")%>% 
    e_title(tit) %>% 
    e_legend() %>% 
    e_legend(orient = "vertical", left = "right", top = "center") %>% #side table
    e_grid(right = 150, bottom = 30)  # Increase this value to add more space
  
}

#TREE MAP 3 LAYER
treemap_3_layer_echart <- function(data, l1, l2, l3) {
  level1 <- data %>%
    count({{ l1 }}, name = "value") %>%
    mutate({{ l2 }} := NA, {{ l3 }} := NA)
  
  level2 <- data %>%
    count({{ l1 }}, {{ l2 }}, name = "value") %>%
    mutate({{ l3 }} := NA)
  
  level3 <- data %>%
    count({{ l1 }}, {{ l2 }}, {{ l3 }}, name = "value")
  
  df <- bind_rows(level3, level2, level1) %>%
    mutate(
      name = coalesce(
        as.character({{ l3 }}),
        as.character({{ l2 }}),
        as.character({{ l1 }})
      ),
      parent = case_when(
        !is.na({{ l3 }}) ~ as.character({{ l2 }}),
        !is.na({{ l2 }}) ~ as.character({{ l1 }}),
        TRUE ~ NA_character_
      )
    ) %>%
    select(name, parent, value)
  
  df %>%
    e_charts() %>%
    e_treemap_(name = "name", value = "value", parents = "parent") %>%
    e_title("Treemap: l1 → l2 → l3") %>%
    e_tooltip(trigger = "item") %>% 
    e_legend() %>% 
    e_legend(orient = "vertical", left = "right", top = "center") %>% #side table
    e_grid(right = 150, bottom = 30)  # Increase this value to add more space
  
}
#Plot Sub Function===================================================================
#Theme Function
theme_function <- function(
    margin_plot = NULL, 
    title_size = 16, 
    font_weight = "bold",
    #input related to x
    margin_x_title = NULL, 
    x_label_position = NULL, #example input: element_text(angle = 45, vjust = 0.5) 
    
    #input related to y
    margin_y_title = NULL,
    
    #thing related to legend
    legend = T #toggle legend
    ){
  
  #Default value for margin
  if(is.null(margin_plot)){
    margin_plot <- margin(5.5,4,4,5.5) #sample
  }
  if(is.null(margin_x_title)){
    margin_x_title <- margin(0,0,0,0) #sample
  }
  if(is.null(margin_y_title)){
    margin_y_title <- margin(0,0,0,0) #sample
  }
  
  #Default value for positoning
  if(is.null(x_label_position)){
    x_label_position = element_text(angle = 0, vjust =0) #sample
  }
  
  #legend
  if(legend == F){
    
    theme(
      axis.text.x = x_label_position,
      
      #margin for plot
      plot.margin = margin_plot,
      plot.title = element_text(size = title_size, face = font_weight, hjust = 0.5),
      #margin for X title
      axis.title.x = element_text(margin = margin_x_title),
      #margin for y title
      axis.title.y = element_text(margin = margin_y_title),
      #legend
      legend.position = "none"
      
    )
    
  }else{
    
    theme(
      axis.text.x = x_label_position,
      
      #margin for plot
      plot.margin = margin_plot,
      plot.title = element_text(size = title_size, face = font_weight, hjust = 0.5),
      #margin for X title
      axis.title.x = element_text(margin = margin_x_title),
      #margin for y title
      axis.title.y = element_text(margin = margin_y_title)
      
    )
    
  }
}


#set label and adjust height
geom_text_function <- function(
    repel = T, #if F then use geom_text
    vjust = NULL,
    hjust = NULL,
    stack = T,
    ...
    ) {
  
  position_setting <- if(stack == T){position_stack()}else{position_dodge(width = 0.9)}
  
  vjust <- if(as.character(enquo(vjust)[2])== "NULL"){-0.5}else {vjust}
  hjust <- if(as.character(enquo(hjust)[2])== "NULL"){0.5}else{hjust}
  
  if(repel == T){
    geom_text_repel(
      aes(...),
      position = position_setting,
      vjust = vjust,
      hjust = hjust
    )
  }else{
    geom_text(
      aes(...),
      position = position_setting,
      vjust = vjust,
      hjust = hjust
    )
  }
}

#scale_Y_continuous
scale_y_continuous_function <- function(
    
  #-----group--------
    start_range = NULL, 
    end_range = NULL, 
  #------------------
    suffix = M, #either m/k
    continuous = T){
  
  surfix_val <- deparse(substitute(suffix))
  
  #Defined how Y axis label work 
  if(str_detect(as.character(enquo(surfix_val)[2]), regex("m", ignore_case = T))){
    fmt <- label_number(scale = 1e-6, suffix = "M")
  }else if(str_detect(as.character(enquo(surfix_val)[2]), regex("k", ignore_case = T))){
    fmt <- label_number(scale = 1e-3, suffix = "K")
  }else{
    fmt <- scales::comma
  }
  
  if(as.character(enquo(start_range)[2]) == "NULL" &
     as.character(enquo(end_range)[2]) == "NULL"){
    
    if(continuous){
      scale_y_continuous(
        labels = fmt
      )
    }else{
      scale_y_discrete(
        labels = fmt
      )
    }
    
  }else{
    
    #calculation start 
    if(continuous){
      scale_y_continuous(
        labels = fmt,
        expand = c(0,0),
        limits = c(start_range,end_range)
      ) 
    }else{
      scale_y_discrete(
        labels = fmt,
        expand = c(0,0),
        limits = c(start_range,end_range)
      )
    }
    
  }
}  
#Scale_X_continuous
scale_x_continuous_function <- function(left_scale, right_scale, discrete = T){
  if(discrete){
    scale_x_discrete(
      expand = expansion(mult = c(left_scale,right_scale)) #0:1
    )
  }else{
    scale_x_continuous(
      expand = expansion(mult = c(left_scale,right_scale)) #0:1
    )
  }
}

