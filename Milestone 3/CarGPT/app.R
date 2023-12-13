library(shiny)
library(httr)
library(stringr)
library(dplyr)

dataset=read.csv("car.csv")

ui <- fluidPage(
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
      ),tags$hr(),
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
    )
    ,
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
      ),style = "background-color: #00A67E")
  ),style = "background-color: #3d3f4e")

server <- function(input, output, session) {
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
}
shinyApp(ui = ui, server = server)
