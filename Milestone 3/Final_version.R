library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)
library(httr)
library(stringr)
library(randomForest)

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

load("./model_forest.RData")
load("./train.RData")
load("./test.RData")

test=test_forest

predictions <- predict(model_forest, test)


ui <- navbarPage(
  title = "Car Analysis and Prediction",
  theme = shinytheme("yeti"),
  
  tabPanel("CarGPT",
           div(
             titlePanel("CarGPT"),
             style = "color: white; background-color: #3d3f4e"
           ),
           sidebarLayout(
             sidebarPanel(
               h3("Welcome to CarGPT"),
               p("This application allows use AI to find your dream car"),
               textInput("api_key", "API Key", "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"),
               tags$p("Find your own OpenAI API:", 
                      tags$a(href = "https://platform.openai.com/account/api-keys", target="_blank", "https://platform.openai.com/account/api-keys")
               ),
               tags$hr(),
               selectInput("model_name", "Model Name",
                           choices = c("gpt-4", "gpt-4-0314", "gpt-3.5-turbo-0301", "gpt-3.5-turbo"), selected = "gpt-3.5-turbo"),
               tags$hr(),
               sliderInput("temperature", "Temperature", min = 0.1, max = 1.0, value = 0.7, step = 0.1),
               sliderInput("max_length", "Maximum Length", min = 1, max = 2048, value = 512, step = 1),
               tags$hr(),
               textAreaInput(inputId = "sysprompt", label = "SYSTEM PROMPT", height = "200px", 
                             value = "I will give you a description of the car that I want, you are going to provide me with the parameters of only one most suitable car for me, and it has to be very very very strictly in this format, the Manufacturer need to be all capital, the first letter of Model will be capital, rest will be normal: Manufacturer:xxx,Model:xxx,Description:xxx", 
                             placeholder = "Enter a new system prompt if you want to change"),
               tags$hr(),
               style = "background-color: #1a1b1f; color: white"
             ),
             mainPanel(
               tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}"),
               tags$style(type = "text/css", ".shiny-output-error:before {content: ' Check your inputs or API key';}"),
               tags$style(type = "text/css", "label {font-weight: bold;}"),
               fluidRow(
                 column(12,tags$h3("Chat History"),tags$hr(),uiOutput("chat_history"),tags$hr())
               ),
               fluidRow(
                 column(11,textAreaInput(inputId = "user_message", placeholder = "Enter your message:", label="USER PROMPT", width = "100%")),
                 column(1,actionButton("send_message", "Send",icon = icon("play"),height = "350px")),
                 tableOutput("filteredCarData")
               ),
               style = "background-color: #00A67E"
             )
           )
  ),
  
  # Used Car Visualization
  tabPanel("Used Car Visualization",
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
  ),
  
  # Car Price Prediction
  tabPanel("Car Price Prediction",
           titlePanel("Car Price Prediction"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("year", "Year", min = min(train$Year), max = max(train$Year), value = round(mean(train$Year))),
               selectInput("category", "Category", choices = unique(train$Category)),
               sliderInput("mileage", "Mileage", min = min(train$Mileage), max = max(train$Mileage), value = round(mean(train$Mileage))),
               selectInput("fuelType", "Fuel Type", choices = unique(train$FuelType)),
               sliderInput("engineVolume", "Engine Volume", min = min(train$EngineVolume), max = max(train$EngineVolume), value = mean(train$EngineVolume)),
               selectInput("driveWheels", "Drive Wheels", choices = unique(train$DriveWheels)),
               selectInput("gearBox", "Gear Box", choices = unique(train$GearBox)),
               selectInput("doors", "Doors", choices = unique(train$Doors)),
               selectInput("wheel", "Wheel", choices = unique(train$Wheel)),
               selectInput("color", "Color", choices = unique(train$Color)),
               selectInput("leatherInterior", "Leather Interior", choices = unique(train$LeatherInterior)),
               selectInput("clearance", "Clearance", choices = unique(train$Clearance)),
               actionButton("predict", "Predict Price")
             ),
             mainPanel(
               textOutput("predictedPrice"),
               DTOutput("similarCarsTable"),
               plotOutput("plotActualVsPredicted"),
               plotOutput("plotResiduals"),
               plotOutput("plotFeatureImportance")
             )
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
    ggplot(df %>% filter(Year >= 1980), aes(x = Year)) + 
      geom_histogram(binwidth = 1) +
      scale_x_continuous(limits = c(1980, max(df$Year, na.rm = TRUE)), 
                         breaks = seq(1980, max(df$Year, na.rm = TRUE), by = 5)) +
      xlab("Year") + ylab("Total Number of Cars") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$topManufacturersHeatmap <- renderPlot({
    dataToDisplay <- if (is.null(input$brush) || input$brush$xmin == input$brush$xmax) {
      df
    } else {
      df %>%
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
      brushedData <- df%>%
        filter(Year >= input$brush$xmin, Year <= input$brush$xmax, Year >= 1980) %>%
        group_by(Year, Manufacturer) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        arrange(Year, desc(count))
      
      brushedData
    }
  }, options = list(lengthChange = FALSE))
  
  
  
  chat_data <- reactiveVal(data.frame())
  # Function to parse the GPT response
  parse_gpt_response <- function(response) {
    # Adjusted regex to allow for optional spaces
    parsed <- str_match(response, "Manufacturer:\\s*([^,]+),\\s*Model:\\s*([^,]+)")
    return(list(manufacturer = trimws(parsed[,2]), model = trimws(parsed[,3])))
  }
  
  
  observeEvent(input$send_message, {
    if (input$user_message != "") {
      new_data <- data.frame(source = "User", message = input$user_message, stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), new_data))
      
      gpt_res <- call_gpt_api(input$api_key, input$user_message, input$model_name, input$temperature, input$max_length, input$sysprompt)
      
      if (!is.null(gpt_res)) {
        gpt_data <- data.frame(source = "ChatGPT", message = gpt_res, stringsAsFactors = FALSE)
        chat_data(rbind(chat_data(), gpt_data))
        
        print(gpt_res)
        # Parse response and filter dataset
        car_specs <- parse_gpt_response(gpt_res)
        
        print(car_specs)
        
        # Reactive expression for filtered data
        filteredData <- reactive({dataset %>% 
            filter(Manufacturer == car_specs$manufacturer , Model == car_specs$model)
        })
        
        # Render filtered data as a table
        output$filteredCarData <- renderTable({
          filteredData()
        })
        
      }
      updateTextInput(session, "user_message", value = "")
    }
  })
  
  
  
  call_gpt_api <- function(api_key, prompt, model_name, temperature, max_length, sysprompt) {
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type("application/json"),
      encode = "json",
      body = list(
        model = model_name,
        messages = list(
          list(role = "user", content = prompt),
          list(role = "system", content = sysprompt)
        ),
        temperature = temperature,
        max_tokens = max_length
      )
    )
    return(str_trim(content(response)$choices[[1]]$message$content))
  }
  
  output$chat_history <- renderUI({
    chatBox <- lapply(1:nrow(chat_data()), function(i) {
      tags$div(class = ifelse(chat_data()[i, "source"] == "User", "alert alert-secondary", "alert alert-success"),
               HTML(paste0("<b>", chat_data()[i, "source"], ":</b> ", chat_data()[i, "message"])))
    })
    do.call(tagList, chatBox)
  })
  
  observeEvent(input$download_button, {
    if (nrow(chat_data()) > 0) {
      session$sendCustomMessage(type = "downloadData", message = "download_data")
    }
  })
  prediction <- eventReactive(input$predict, {
    # Create a data frame from the input values
    new_data <- data.frame(
      Year = as.numeric(as.character(input$year)),
      Category = input$category,
      Mileage = as.numeric(input$mileage),
      FuelType = input$fuelType,
      EngineVolume = as.numeric(input$engineVolume),
      DriveWheels = input$driveWheels,
      GearBox = input$gearBox,
      Doors = input$doors,
      Wheel = input$wheel,
      Color = input$color,
      LeatherInterior = input$leatherInterior,
      Clearance = input$clearance
    )
    
    # Convert categorical features to factors with the same levels as in training data
    new_data$Category <- factor(new_data$Category, levels = levels(train$Category))
    new_data$FuelType <- factor(new_data$FuelType, levels = levels(train$FuelType))
    new_data$DriveWheels <- factor(new_data$DriveWheels, levels = levels(train$DriveWheels))
    new_data$GearBox <- factor(new_data$GearBox, levels = levels(train$GearBox))
    new_data$Doors <- factor(new_data$Doors, levels = levels(train$Doors))
    new_data$Wheel <- factor(new_data$Wheel, levels = levels(train$Wheel))
    new_data$Color <- factor(new_data$Color, levels = levels(train$Color))
    new_data$LeatherInterior <- factor(new_data$LeatherInterior, levels = levels(train$LeatherInterior))
    new_data$Clearance <- factor(new_data$Clearance, levels = levels(train$Clearance))
    
    # Predict using the model
    predicted_price <- predict(model_forest, new_data)
    return(predicted_price)
  })
  
  output$predictedPrice <- renderText({
    req(prediction())
    paste("Predicted Price: $", round(prediction(), 2))
  })
  
  output$similarCarsTable <- renderDT({
    req(prediction())
    predicted_price <- prediction()
    
    # Define a price range for similar cars
    price_range <- 1000  # Adjust this value based on your data and needs
    lower_bound <- predicted_price - price_range
    upper_bound <- predicted_price + price_range
    
    # Filter training data for similar cars
    similar_cars <- train[train$Price >= lower_bound & train$Price <= upper_bound, ]
    datatable(similar_cars, options = list(pageLength = 5))
  }, server = FALSE)
  
  output$plotActualVsPredicted <- renderPlot({
    actual_vs_predicted <- data.frame(Actual = test$Price, Predicted = predictions)
    ggplot(actual_vs_predicted, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      labs(title = "Actual vs Predicted Prices", x = "Actual Price", y = "Predicted Price")
  })
  
  output$plotResiduals <- renderPlot({
    actual_vs_predicted <- data.frame(Actual = test$Price, Predicted = predictions)
    actual_vs_predicted$Residuals <- actual_vs_predicted$Actual - actual_vs_predicted$Predicted
    ggplot(actual_vs_predicted, aes(x = Predicted, y = Residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      labs(title = "Residuals vs Predicted", x = "Predicted Price", y = "Residuals")
  })
  
  output$plotFeatureImportance <- renderPlot({
    importance <- randomForest::importance(model_forest)
    featureImportance <- data.frame(Feature=row.names(importance), Importance=importance[,1])
    ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Feature Importance in Predicting Car Prices")
  })
}
shinyApp(ui = ui, server = server)