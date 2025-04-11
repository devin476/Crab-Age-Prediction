library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(caret)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  #Font Correction (to work with Dark Theme)
  tags$head(
    tags$style(HTML("
    .dataTables_wrapper {
      color: #ffffff;
    }
    table.dataTable {
      color: #ffffff !important;
    }
    table.dataTable td, table.dataTable th {
      color: #ffffff !important;
    }
    .dataTables_filter input {
      color: #ffffff;
      background-color: #2c2c2c;
    }
    .dataTables_length select {
      color: #ffffff;
      background-color: #2c2c2c;
    }
  "))
  ),
  titlePanel( title = div(icon("chart-line"), "Crab Measurement Comparison Against Age 🦀"),
              windowTitle = "Crab App"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("xvar", "Select variable to compare with Age:",
                  choices = NULL, multiple = FALSE),
      checkboxInput("showReg", "Add regression line", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Data Table", DTOutput("table")),
        tabPanel("Predict Age",
                 h4("Input crab measurements:"),
                 fluidRow(
                   column(4, numericInput("Shucked", "Shucked Weight", value = NA)),
                   column(4, numericInput("Height", "Height", value = NA)),
                   column(4, numericInput("Diameter", "Diameter", value = NA))
                 ),
                 fluidRow(
                   column(4, numericInput("Length", "Length", value = NA)),
                   column(4, numericInput("Weight", "Weight", value = NA))
                 ),
                 br(),
                 actionButton("predictBtn", "Estimate Age", icon = icon("magic")),
                 br(), br(),
                 verbatimTextOutput("agePrediction")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #Load data
  CrabTrain <- read.csv("Data/train-1.csv")
  
  #Select relevant features
  feature_df <- CrabTrain %>%
    select(Shucked.Weight, Height, Diameter, Length, Weight, Age)
  
  #Split for consistency with Model
  trainIndex <- createDataPartition(feature_df$Age, p = 0.7, list = FALSE)
  train_raw <- feature_df[trainIndex, ]
  
  #Preprocess/standardize 
  preproc <- preProcess(train_raw[, -6], method = c("center", "scale"))  # exclude Age
  train_scaled <- predict(preproc, train_raw[, -6])
  train_data <- cbind(train_scaled, Age = train_raw$Age)
  
  #Fit linear regression model
  lm_model <- lm(Age ~ Shucked.Weight + Height + Diameter + Length + Weight, data = train_data)
  
  #Set variables for dropdown
  compare_vars <- setdiff(names(CrabTrain), c("id", "Age", "Sex"))
  display_names <- gsub("\\.", " ", compare_vars)
  updatePickerInput(session, "xvar", choices = setNames(compare_vars, display_names))
  
  #Scatter plot
  output$scatterPlot <- renderPlot({
    req(input$xvar)
    
    p <- ggplot(CrabTrain, aes_string(x = "Age", y = input$xvar)) +
      geom_point(alpha = 0.6, color = "#2c3e50") +
      labs(title = paste("Age vs", input$xvar),
           x = "Age",
           y = input$xvar) +
      theme_minimal()
    
    if (input$showReg) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "#e74c3c")
    }
    
    p
  })
  
  #Summary tab
  output$summary <- renderPrint({
    summary(CrabTrain[, !(names(CrabTrain) %in% c("id", "Sex"))])
  })
  
  #Data table
  output$table <- renderDT({
    datatable(CrabTrain, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #Age prediction
  observeEvent(input$predictBtn, {
    req(input$Shucked, input$Height, input$Diameter, input$Length, input$Weight)
    
    #User input to dataframe
    new_input <- data.frame(
      Shucked.Weight = input$Shucked,
      Height         = input$Height,
      Diameter       = input$Diameter,
      Length         = input$Length,
      Weight         = input$Weight
    )
    
    #Apply training preprocessing
    new_scaled <- predict(preproc, new_input)
    
    #Predict
    pred <- predict(lm_model, newdata = new_scaled)
    
    output$agePrediction <- renderText({
      if (pred < 0) {
        "Estimated Age: < 0 (check input values)"
      } else {
        paste("Estimated Age:", round(pred, 2))
      }
    })
  })
}

shinyApp(ui, server)
