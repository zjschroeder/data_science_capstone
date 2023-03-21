library(shiny)
library(tidyverse)
library(shinythemes)
library(broom)
library(sjPlot)
data <- readRDS(here::here("static/data/cc.RDS"))

# Define UI for Shiny app
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Cancel Culture"),
  tabsetPanel(
    tabPanel("Identifying Tweet Topics",
             sidebarLayout(
               sidebarPanel(
                 
               ),
               mainPanel(
                 tableOutput("max_table")
               )
             )
    ),
    tabPanel("Play with Models",
             conditionalPanel(
               condition = "length($('select[multiple]').val()) > 0",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("var_select", "Select predictors", 
                               choices = names(data),
                               multiple = TRUE),
                   actionButton("run_model", "Run")
                 ),
                 mainPanel(
                   uiOutput("lm_output")
                 )
               )
             )
    )
  )
)

# Define server for Shiny app
server <- function(input, output) {
  
  # Reactive function to generate maximum value and corresponding text for each influencer
  max_vals <- reactive({
    data %>%
      filter(!is.na(!!sym(input$column_select))) %>%
      # group_by(Influencer) %>%
      top_n(n = floor(0.01 * n()), wt = !!sym(input$column_select)) %>%
      slice_max(nchar(text)) %>% 
      select(text)
  })
  
  # Render the maximum values and corresponding text as a table
  output$max_table <- renderTable({
    max_vals()
  })
  
  # Reactive function to run linear model and output summary
  lm_output <- eventReactive(input$run_model, {
    # Filter data to only include selected variables
    df <- data %>% select(input$var_select, Retweets)
    
    # Run linear model and output summary
    model <- lm(Retweets ~ ., data = df)
    model_summary <- summary(model)
    
    # Generate plot of model output using emmeans::plot_model
    plot_model(model, type = "pred")
    
    # Return the model summary and plot
    list(summary = model_summary, plot = plot_model(model, type = "pred"))
  })
  
  # Render linear model output as text and plot
  output$lm_output <- renderUI({
    model_output <- lm_output()
    
    tagList(
      h3("Model Summary"),
      verbatimTextOutput("summary_output"),
      h3("Model Plot"),
      plotOutput("plot_output")
    )
    
  })
  
  # Render model summary as text
  output$summary_output <- renderPrint({
    lm_output()$summary
  })
  
  # Render model plot
  output$plot_output <- renderPlot({
    lm_output()$plot
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
