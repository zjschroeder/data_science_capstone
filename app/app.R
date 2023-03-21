library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(broom)
library(sjPlot)

##### Loading/Sourcing
data <- readRDS(here::here("data/cc.RDS"))

source(here::here("scripts/meta_data.R"))

select_options <- useful_names[10:40]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "A Multi-faceted Investigation of Cancel Culture"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tweet Scraping", tabName = "tab1"),
      menuItem("Coding and Cleaning", tabName = "tab2"),
      menuItem("Exploring Linguistic Features", tabName = "tab3"),
      menuItem("Exploring Predictors of Tweet Engagement", tabName = "tab4"),
      menuItem("Menu Item 5", tabName = "tab5")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab1",
              uiOutput("scraping_tweets_md")),
      tabItem(tabName = "tab2",
              uiOutput("coding_and_cleaning_md")),
      tabItem(tabName = "tab3",
              fluidRow(
                box(title = "Box 3",
                    selectInput("column_select", "Please select a tweet feature", 
                                choices = select_options)),
                box(title = "Example Tweet",
                    tableOutput("max_table"))
              )),
      tabItem(tabName = "tab4",
        fluidRow(
          conditionalPanel(
            condition = "length($('select[multiple]').val()) > 0",
            box(
              selectInput("var_select", "Select predictors", 
                          choices = select_options,
                          multiple = TRUE),
              actionButton("run_model", "Run")
            )
          )
        ),
        fluidRow(
          box(
          uiOutput("lm_output")
          )
        )
      )
      # tabItem(tabName = "tab5",
      #         uiOutput("scraping_tweets_md")),
    )
  )
)


# Define server
server <- function(input, output) {
  output$plot1 <- renderPlot({
    # plot data here
  })
  output$table1 <- renderTable({
    # create table here
  })
  output$plot2 <- renderPlot({
    # plot data based on input$select1 here
  })
  output$scraping_tweets_md <- renderUI({
    HTML(markdown::markdownToHTML(
      knitr::knit(here::here("scripts/scraping_twitter_data.Rmd"))
    ))
  })
  output$coding_and_cleaning_md <- renderUI({
    HTML(markdown::markdownToHTML(
      knitr::knit(here::here("scripts/coding_and_cleaning_tweets.Rmd"))
    ))
  })
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

# Run the app
shinyApp(ui = ui, server = server)
