#setwd("~/Desktop/data_viz/hw3/test")

packages <- c('shiny', 'ggvis', 'GGally', 'ggplot2', 'sqldf')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(packages, library, character.only = TRUE)
source('utils.R')

ui <- fluidPage(
  titlePanel("Facebook Metrics Visualizations"),
  tabsetPanel(
    ###########################################
    ######## Bubble Chart #####################
    ###########################################
    tabPanel('Bubble Plot',
             sidebarLayout(
               sidebarPanel(width = 2,
                 checkboxGroupInput('bubble_type', 
                                    label = h5('Interaction Type'),
                                    choices = list('comments', 'likes', 'shares'),
                                    selected = 3),
                 sliderInput('month_num',
                             label = h5('Month'),
                             min = 1,max = 12,value = 1,animate = T)
               ),
               mainPanel(uiOutput('ggvis_ui'),
                         ggvisOutput('ggvis'))
             )),
    ###########################################
    ######## Scatterplot Matrix ###############
    ###########################################    
    tabPanel('Scatterplot Matrix',
             sidebarLayout(
               sidebarPanel(width = 2,
                 checkboxGroupInput('mat_col',
                                    label = h5('Scatter Matrix'),
                                    choices = list('Month', 'Weekday', 'Hour', 'NumComments', 'NumLikes', 'NumShares', 'TotalInteractions'),
                                    selected = list('Month', 'TotalInteractions'))
               ),
               mainPanel(plotOutput('scat_plot'))
             )),
    ###########################################
    ######## Parallel Plot ####################
    ###########################################  
    tabPanel('Parallel Coordinates Plot',
             sidebarLayout(
             sidebarPanel(width = 2, checkboxGroupInput('para_type',
                                label = h5('Interaction Type'),
                                choices = list('Links' = 'Link', 
                                               'Photos' = 'Photo', 
                                               'Status' = 'Status', 
                                               'Videos' = 'Video')
                                )),
             mainPanel(plotOutput('para_plot')))
             )
    ))

server <- function(input, output) {
  df <- load_data()
  ###########################################
  ######## Bubble Chart #####################
  ###########################################
  bubble_df <- reactive({
    subset_bubbles <- df[,which(names(df) %in% c('month', 'comment', 'like', 'share'))]
    subset_bubbles_agg <- sqldf('SELECT SUM(comment) as comments,
                                SUM(like) as likes,
                                SUM(share) as shares,
                                month FROM subset_bubbles GROUP BY month')
    melted_agg <- melt(subset_bubbles_agg, id = 'month')
    colnames(melted_agg) <- c('month', 'interaction', 'value')
    
    if(is.null(input$bubble_type)){
      return(melted_agg[melted_agg$month == input$month_num,])
    }
    subset_b <- melted_agg[melted_agg$interaction %in% input$bubble_type,]
    subset_m <- subset_b[subset_b$month == input$month_num,]
    return (subset_m)
  })
  
  bubble_vis <- reactive({
    month_labels <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
    bubble_df() %>% 
    ggvis(x=~month, y=~value, fill=~interaction) %>%
      layer_points(size := 300, stroke :='black') %>%
      scale_numeric('x', domain = c(1,12), nice = F, clamp = T) %>%
      scale_numeric('y', domain = c(104, 14171)) %>% 
      add_axis('x', title = 'Month') %>%
      add_axis('y', title = 'Interaction Count', title_offset=50) %>%
      add_legend("fill",title="Interaction Type") %>%
      add_tooltip(function(data){
        paste0("Month: ", month_labels[data$month],"<br>",
               "Count: ", data$value,"<br>")
      }, 'hover') 
  })
  
  bubble_vis %>% bind_shiny('ggvis', 'ggvis_ui')
  
  ###########################################
  ######## Scatterplot Matrix ###############
  ###########################################
  scatter_data <- scatter_matrix_data()
  
  scatter_df <- reactive({
    if(is.null(input$mat_col)){
      return(scatter_data)
    }
    subset_m <- scatter_data[,which(names(scatter_data) %in% input$mat_col)]
    if (ncol(subset_m) < 2){
      return(scatter_data)
    }
    return (subset_m)
  })
  
  output$scat_plot <- renderPlot({
    df <- scatter_df()
    ggpairs(df)
  })
  
  ###########################################
  ######## Parallel Plot ####################
  ########################################### 
  
  para_df <- reactive({
    subset_type <- df[,which(names(df) %in% c('type', 'comment', 'like', 'share'))]
    
    if(is.null(input$para_type)){
      return (subset_type)
    }
    subset_i <- df[df$type %in% input$para_type,]
    return (subset_i)
  })
 
  output$para_plot <- renderPlot({
    para_sub <- para_df()
    cols <- which(colnames(para_sub) %in% c('comment', 'like', 'share'))
    t <- which(colnames(para_sub) == 'type')
    ggparcoord(data = para_sub, columns = cols, groupColumn = t, scale = 'globalminmax')+
    ylim(0,2000) + xlab("") + ylab("Count") + theme(legend.position = c(.9,.8), 
                                                    legend.title = element_blank(),
                                                    legend.text = element_text(size = 15),
                                                    legend.background = element_rect(fill = 'transparent'),
                                                    axis.title = element_text(size = 18), 
                                                    axis.text = element_text(size = 15),
                                                    plot.title = element_text(size = 18, hjust = .5)) +
      ggtitle('Interactions by Type') + guides(colour = guide_legend(override.aes = list(size=3)))
  })
  
}

shinyApp(ui = ui, server = server)














