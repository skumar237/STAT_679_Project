library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
carData <- read.csv("679data.csv") 
carData <- carData %>% filter(Year != 0)
ui <- fluidPage(
  titlePanel("Car Data Visualization"),
  splitLayout(
    cellWidths = c("40%", "60%"),
    plotOutput("histogram", brush = "brush"),
    plotOutput("topManufacturersHeatmap")
  ),
  DTOutput("brushedDataTbl")
)


server <- function(input, output, session) {
    output$histogram <- renderPlot({
    ggplot(carData %>% filter(Year >= 1980), aes(x = Year)) + 
      geom_histogram(binwidth = 1) +
      scale_x_continuous(limits = c(1980, max(carData$Year, na.rm = TRUE)), 
                         breaks = seq(1980, max(carData$Year, na.rm = TRUE), by = 5)) +
      xlab("Year") + ylab("Total Number of Cars") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$topManufacturersHeatmap <- renderPlot({
    dataToDisplay <- if (is.null(input$brush) || input$brush$xmin == input$brush$xmax) {
      carData
    } else {
      carData %>%
        filter(Year >= input$brush$xmin, Year <= input$brush$xmax)
    }
    
    topManufacturersData <- dataToDisplay %>%
      group_by(Manufacturer) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      arrange(desc(count)) %>%
      slice_max(order_by = count, n = 20)
    
    ggplot(topManufacturersData, aes(x = reorder(Manufacturer, -count), y = factor(1), fill = count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none") +
      labs(x = "Manufacturer", y = "", fill = "Number of Cars")
  })
  
  output$brushedDataTbl <- renderDataTable({
    if (is.null(input$brush) || input$brush$xmin == input$brush$xmax) {
      data.frame(Manufacturer = character(0), Year = integer(0), Count = integer(0))
    } else {
      brushedData <- carData %>%
        filter(Year >= input$brush$xmin, Year <= input$brush$xmax, Year >= 1980) %>%
        group_by(Year, Manufacturer) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        arrange(Year, desc(count))
      
      brushedData
    }
  }, options = list(lengthChange = FALSE))
}

shinyApp(ui = ui, server = server)
