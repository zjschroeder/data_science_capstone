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
      menuItem("Overview", tabName = "home"),
      menuItem("Data Collection", tabName = "tab1"),
      menuItem("Linguistic Features", tabName = "tab3"),
      menuItem("Predictors of Engagement", tabName = "tab4"),
      menuItem("Timelines", tabName = "tab5")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidPage(htmlOutput("overview"))
              ),
      tabItem(tabName = "tab1",
              fluidPage(htmlOutput("data_collection"))
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
                )),
                fluidRow(box(width = 12,
                    plotOutput("plot_output"))
              ),
              fluidRow(box(width = 12,
                           textOutput("summary_output"))
                
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
  
  output$overview <- renderUI({includeHTML(here::here("overview.html"))
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
  
  # Define the reactive model based on the input variables selected
  model <- reactive({
    if(length(input$var_select) > 0) {
      df <- data %>% 
        select(input$var_select, Retweets)
      # fit the linear model
      lm(Retweets ~ ., data = df)
    }
  })
  
  # Render the plot based on the model
  output$plot_output <- renderPlot({
    if(!is.null(model())) {
      plot_model(model(), type = "pred")
    }
  })
  
  # Render the summary based on the model
  output$summary_output <- renderPrint({
    if(!is.null(model())) {
      summary(model())
    }
  })
  
  # PAGE 5 Render timeline output
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