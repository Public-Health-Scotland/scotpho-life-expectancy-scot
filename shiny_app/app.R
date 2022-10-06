# Code to create shiny chart of annual changes in life expectancy and healthy life expectancy by sex
# This is published in the following section of the ScotPHO website: 
# Population Dynamics > Deaths and life expectancy > Data > Scotland


############################.
## Global ----
############################.
############################.
##Packages 

library(dplyr) #data manipulation
library(plotly) #charts
library(shiny) #shiny apps
library(readr) #reading csvs

# Data file
le_hle_data <- read_csv("data/change_le_hle_scotland.csv")

############################.
## Visual interface ----
############################.
#Height and widths as percentages to allow responsiveness
ui <- fluidPage(style="width: 650px; height: 500px; ",
                div(style= "width:100%",
                    h4("Chart 2. Annual changes in life expectancy and healthy life expectancy in Scotland"),
                    div(style = "width: 50%; float: left;",
                        selectInput("measure", label = "Select a measure type",
                                    choices = c("Life expectancy", "Healthy life expectancy"), 
                                    selected = "Life expectancy"))),
                
                # Suggest removing this filter
                # div(style = "width: 40%; float: left;",
                #     selectInput("difference", label = "Difference from previous year",
                #                 choices = c("Years", "Weeks"), 
                #                 selected = "Years")),
                
                div(style = "width: 25%; float: left;",
                    selectInput("sex", label = "Select sex",
                                choices = c("Male", "Female"), 
                                selected = "Male", multiple = T)),
                
                
                div(style= "width:100%; float: left;", #Main panel
                    plotlyOutput("chart", width = "100%", height = "350px"),
                    p(div(style = "width: 25%; float: left;", #Footer
                          HTML("Source: <a href='https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy' target='_blank'>NRS</a>")),
                      div(style = "width: 25%; float: left;",
                          downloadLink('download_data', 'Download data')))
                    )
                ) # fluidPage

############################.
## Server ----
############################.
server <- function(input, output) {
  
  output$chart <- renderPlotly({
    
    # Data
    chart_data <- le_hle_data  %>% 
      filter(Measure == input$measure & Difference == "Years"
               #Difference == input$difference 
             & Sex %in% input$sex
      )
    
    # Information to be displayed in tooltip
    tooltip <- c(paste0("Time period (3 year average): ", chart_data$Time_period, "<br>",
                        "Difference from previous year (in years): ", chart_data$Value, "<br>"))
                        #"Difference from previous year in ", input$difference, ": ", chart_data$Value, "<br>"))
    
    # y-axis title - add to layout if not dynamic
    yaxistitle <- paste0("Annual change in years")
    #yaxistitle <- paste0("Annual change in ", input$difference)
    
    # Define line colours
    pal <- c('#0078D4', '#1E7F84')
    
    # Define number of lines on chart
    num <- length(unique(chart_data$Sex))
    
    # Buttons to remove from plot
    bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',
                         'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',
                         'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')
    
    
    plot <- plot_ly(data = chart_data, x=~Time_period, y = ~Value, 
                    color= ~Sex, colors = pal[1:num], 
                    type = "scatter", mode = 'lines+markers', 
                    symbol= ~Sex, symbols = list('circle','square'), marker = list(size= 7),
                    width = 650, height = 350,
                    text=tooltip, hoverinfo="text") %>%  
        
     # Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
             yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
             xaxis = list(title = "3 year average",  fixedrange=TRUE, tickangle = 270),  
             font = list(family = 'Arial, sans-serif'), #font
             margin = list(pad = 4, t = 50), #margin-paddings
             hovermode = 'false',  # to get hover compare mode as default
             legend = list(orientation = "h", x=0, y=1.2)) %>% 
      config(displayModeBar= T, displaylogo = F, editable =F, modeBarButtonsToRemove = bttn_remove) 
    # taking out plotly logo and collaborate button
    
  }) 
  
  
  # Allow user to download data
  output$download_data <- downloadHandler(
    filename =  'annual_changes_in_le_and_hle_data.csv', 
    content = function(file) {
      write.csv(le_hle_data, file, row.names=FALSE) })
  
} # end of server

############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)

##END