library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(broom)
library(sjPlot)
library(plotly)
library(lubridate)
library(rdrop2)

data <- drop_read_csv("Apps/Cancel Culture Access/cc.csv")

data <- data[,-1]
names(data) <- c(useful_names[1:40], "Date")

source(here::here("scripts/meta_data.R"))

select_options <- useful_names[10:40]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Investigating Cancel Culture",
                  titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Collection", tabName = "tab1"),
      menuItem("Linguistic Features", tabName = "tab3"),
      menuItem("Predictors of Engagement", tabName = "tab4"),
      menuItem("Timelines", tabName = "tab5")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab1",
              fluidRow(htmlOutput("data_collection")

              )
      ),
      tabItem(tabName = "tab3",
              fluidRow(
                box(width = 12,
                    selectInput("column_select", 
                                "Please select a tweet feature", 
                                choices = select_options)
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Example Tweet",
                    tableOutput("max_table")
                )
              )
      ),
      tabItem(tabName = "tab4",
              fluidRow(
                box(width = 12,
                    conditionalPanel(
                      condition = "length($('select[multiple]').val()) > 0",
                      selectInput("var_select", "Select predictors", 
                                  choices = select_options,
                                  multiple = TRUE),
                      actionButton("run_model", "Run")
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                    uiOutput("lm_output")
                )
              )
      ),
      tabItem(tabName = "tab5",
              fluidRow(
                plotlyOutput(outputId = "timeline_plot")
              )
      )
    )
  )
)


# Define server
server <- function(input, output) {
  output$data_collection <- renderUI({includeHTML(here::here("scraping_twitter_data.html"))
    })
  
  max_vals <- reactive({
    data %>%
      filter(!is.na(!!sym(input$column_select))) %>%
      # group_by(Influencer) %>%
      top_n(n = floor(0.01 * n()), wt = !!sym(input$column_select)) %>%
      slice_max(nchar(text), n = 5) %>% 
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
  
  # PAGE 5
  
  timeline <- eventReactive(input$run_timeline, {
    
    
    fig <- plot_ly(x = df$week_date,
                   type = "histogram")
  })
  
  # Render timeline output
  df <- data %>% 
    select(Date) %>% 
    mutate(
      week_date = round_date(ymd_hms(data$Date), unit = "week")
    )
  
  timeline_fig <- plot_ly(x = df$week_date,
                          type = "histogram")
  
  output$timeline_plot <- renderPlotly({
    timeline_fig
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)