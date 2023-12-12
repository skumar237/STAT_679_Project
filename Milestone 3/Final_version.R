library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes) 

df <- read.csv("car.csv")
df <- na.omit(df)
df$Year <- as.integer(df$Year)
df$Manufacturer <- as.character(df$Manufacturer)
df$Model <- as.character(df$Model)
df <- df %>% 
  filter(EngineVolume > 1 & EngineVolume < 10, Price > 1000 & Price < 300000)
df$Manufacturer <- tolower(df$Manufacturer)
df$Model <- tolower(df$Model)
df$CarAge <- 2020 - df$Year

country_coords <- data.frame(
  Country = c("USA", "Germany", "Japan", "Italy", "United_Kingdom", "France", "South_Korea", "China", "Russia", "Sweden"),
  Latitude = c(37.0902, 51.1657, 36.2048, 41.8719, 55.3781, 46.2276, 35.9078, 35.8617, 61.5240, 60.1282),
  Longitude = c(-95.7129, 10.4515, 138.2529, 12.5674, -3.4360, 2.2137, 127.7669, 104.1954, 105.3188, 18.6435),
  stringsAsFactors = FALSE
)

# Define countries and corresponding car brands
brands_by_country <- list(
  USA = c("BUICK", "CADILLAC", "CHEVROLET", "CHRYSLER", "DODGE", "FORD", "GMC", "HUMMER", "JEEP", "LINCOLN", "MERCURY", "PONTIAC", "SATURN", "TESLA"),
  Germany = c("AUDI", "BMW", "MERCEDES-BENZ", "OPEL", "PORSCHE", "VOLKSWAGEN"),
  Japan = c("ACURA", "DAIHATSU", "HONDA", "INFINITI", "ISUZU", "LEXUS", "MAZDA", "MITSUBISHI", "NISSAN", "SCION", "SUBARU", "SUZUKI", "TOYOTA"),
  Italy = c("ALFA ROMEO", "FIAT", "LAMBORGHINI", "LANCIA", "MASERATI"),
  United_Kingdom = c("ASTON MARTIN", "BENTLEY", "JAGUAR", "LAND ROVER", "MG", "MINI"),
  France = c("CITROEN", "PEUGEOT", "RENAULT"),
  South_Korea = c("DAEWOO", "HYUNDAI", "KIA", "SSANGYONG"),
  China = c("CHERY", "FOTON", "GREATWALL", "HAVAL"),
  Russia = c("GAZ", "UAZ", "VAZ"),
  Sweden = c("SAAB", "VOLVO"),
  Other = c("Other")
)

df$Country <- sapply(df$Manufacturer, function(manufacturer) {
  country <- "Other" 
  for (c in names(brands_by_country)) {
    if (toupper(manufacturer) %in% brands_by_country[[c]]) {
      country <- c
      break
    }
  }
  return(country)
})



ui <- fluidPage(
  theme = shinytheme("yeti"), 
  titlePanel("Used Car Visualization"),
  tabsetPanel(
    tabPanel("World Car Brands Map",
             leafletOutput("map"),
             selectInput("country", "Select Country", choices = names(brands_by_country)),
             selectInput("manufacturer", "Select Manufacturer", choices = NULL),
             plotlyOutput("pricePlot"),
             verbatimTextOutput("selectedInfo"),
             DTOutput("table")
    ),
    tabPanel("Car Model Comparison",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("model1", "Select First Model:", choices = NULL),
                 selectizeInput("model2", "Select Second Model:", choices = NULL)
               ),
               mainPanel(
                 plotlyOutput("comparisonPlot"),
                 DTOutput("modelDetailsTable")
               )
             )
    ),
    tabPanel("Fuel Type",
             sidebarLayout(
               sidebarPanel(
                 selectInput("fuelType", "Select Fuel Type", choices = unique(df$FuelType))
               ),
               mainPanel(
                 plotlyOutput("priceDistributionPlot"),
                 textOutput("descriptionText")
               )
             )
    ),
    tabPanel("Car Data Visualization",
             splitLayout(
               cellWidths = c("40%", "60%"),
               plotOutput("histogram", brush = "brush"),
               plotOutput("topManufacturersHeatmap")
             ),
             DTOutput("brushedDataTbl")
    )
  )
)



server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(country_coords) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = ~Country, group = "country_markers", layerId = ~Country)
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    updateSelectInput(session, "country", selected = click$id)
  })
  
  observeEvent(input$country, {
    manufacturers_in_country <- df %>%
      filter(Country == input$country) %>%
      `$`('Manufacturer') %>%
      unique()
    updateSelectInput(session, "manufacturer", choices = manufacturers_in_country)
  })
  
  output$pricePlot <- renderPlotly({
    req(input$manufacturer)
    filtered_data <- df %>% filter(Manufacturer == input$manufacturer)
    plot <- ggplot(filtered_data, aes(x = Price)) +
      geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
      theme_minimal() +
      labs(title = "Price Distribution", x = "Price", y = "Count")
    ggplotly(plot)
  })
  
  output$table <- renderDT({
    req(input$manufacturer)
    df %>% filter(Manufacturer == input$manufacturer)
  })
  
  observe({
    updateSelectizeInput(session, "model1", choices = unique(df$Model), server = TRUE)
    updateSelectizeInput(session, "model2", choices = unique(df$Model), server = TRUE)
  })
  
  output$comparisonPlot <- renderPlotly({
    validate(
      need(input$model1 %in% df$Model, "Please select a valid first model."),
      need(input$model2 %in% df$Model, "Please select a valid second model.")
    )
    data_model1 <- filter(df, Model == input$model1)
    data_model2 <- filter(df, Model == input$model2)
    p <- ggplot() +
      geom_boxplot(data = data_model1, aes(x = 'Model 1', y = Price), fill = 'blue') +
      geom_boxplot(data = data_model2, aes(x = 'Model 2', y = Price), fill = 'red') +
      labs(title = "Price Comparison Between Models", x = "", y = "Price") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$modelDetailsTable <- renderDT({
    req(input$model1, input$model2)
    details_model1 <- df %>% 
      filter(Model == input$model1) %>%
      summarize(Manufacturer = first(Manufacturer),
                Model = first(Model),
                FuelType = first(FuelType),
                Doors = first(Doors),
                Wheel = first(Wheel),
                Color = first(Color),
                MedianPrice = median(Price, na.rm = TRUE),
                MedianMileage = median(Mileage, na.rm = TRUE),
                MedianEngineVolume = median(EngineVolume, na.rm = TRUE))
    details_model2 <- df %>% 
      filter(Model == input$model2) %>%
      summarize(Manufacturer = first(Manufacturer),
                Model = first(Model),
                FuelType = first(FuelType),
                Doors = first(Doors),
                Wheel = first(Wheel),
                Color = first(Color),
                MedianPrice = median(Price, na.rm = TRUE),
                MedianMileage = median(Mileage, na.rm = TRUE),
                MedianEngineVolume = median(EngineVolume, na.rm = TRUE))
    combined_details <- rbind(details_model1, details_model2)
    datatable(combined_details, options = list(pageLength = 5, searching = FALSE))
  })
  
  output$priceDistributionPlot <- renderPlotly({
    data <- df %>% filter(FuelType == input$fuelType)
    p <- ggplot(data, aes(x = Price)) +
      geom_histogram(binwidth = 1000, fill = "blue") +
      theme_minimal() +
      labs(title = paste("Price Distribution for", input$fuelType, "Cars"), x = "Price", y = "Count")
    ggplotly(p)
  })
  
  output$descriptionText <- renderText({
    paste("The graph above shows the distribution of prices for cars that use", input$fuelType, "as fuel.")
  })
  
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


shinyApp(ui, server)
