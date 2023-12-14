library(shiny)
library(randomForest)
library(DT)
library(ggplot2)

# Load your trained Random Forest model
# Replace the path with the location of your saved model

load("./model_forest.RData")
load("./train.RData")
load("./test.RData")

test=test_forest

predictions <- predict(model_forest, test)

# Define the user interface for the app
ui <- fluidPage(
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

# Define server logic
server <- function(input, output) {
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

# Run the application 
shinyApp(ui, server)