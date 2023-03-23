library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(broom)
library(sjPlot)
library(plotly)
library(lubridate)
source(here::here("scripts/meta_data.R"))

files <- c(here::here("data_1.RDS"), here::here("data_2.RDS"))

data  <- files %>%
  map_dfr(readRDS)

data <- data[,-1]
names(data) <- c(useful_names[1:40], "Date")
dates <- readRDS(here::here("cc_mentions_dates.RDS")) %>% tibble()
names(dates) = "date"

select_options <- useful_names[10:40]

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Investigating Cancel Culture",
                  titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "home"),
      menuItem("Data Collection", tabName = "tab1"),
      menuItem("Timelines", tabName = "tab5"),
      menuItem("Linguistic Features", tabName = "tab3"),
      menuItem("Predictors of Engagement", tabName = "tab4")
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
              fluidRow(box(width = 12, 
                           textOutput("table_description"))),
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
                box(width = 5,
                    textOutput("model_description")),
                box(width = 5,
                    conditionalPanel(
                      condition = "length($('select[multiple]').val()) > 0",
                      selectInput("var_select", "Select predictors", 
                                  choices = select_options,
                                  multiple = TRUE))
                    )
                ),
                fluidRow(box(width = 12,
                    plotOutput("plot_output"))
              ),
              fluidRow(box(width = 12,
                           verbatimTextOutput("summary_output"))
                
              )
      ),
      tabItem(tabName = "tab5",
              fluidRow(box(width = 12,
                plotlyOutput(outputId = "timeline_plot"))
              ),
              fluidRow(
                box(width = 12, 
                    textOutput("timeline_discussion")))
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
  
  output$table_description <- renderText({
    "Using LIWC-22, we coded for 31 linguistic features. These include the amount of analytic thinking, uses of different pronouns, and how polite the language is. Using the drop-down menu below, select features you find interesting and you'll be shown tweets that scored in the top 1% of that feature"
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
      df <- data  %>% 
        select(Retweets, input$var_select)
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
  
 output$model_description <- output$timeline_discussion <- renderText({
   "Now that you've gotten a good idea of what features we coded for, feel free to explore how they each predict the number of retweets a tweet about cancel culture gets"
 })
  # PAGE 5 Render timeline output
  dates <- dates %>% 
    mutate(
      week_date = round_date(ymd_hms(dates$date), unit = "week")
    )
  
  timeline_fig <- plot_ly(x = dates$week_date,
                          type = "histogram")
  
  output$timeline_plot <- renderPlotly({
    timeline_fig
  })
  output$timeline_discussion <- renderText({
    "Cancel Culture is a very recent phenomenon. As is seen above, discourse in earnest began in late 2019. The large spike in tweets discussing cancel culture in July 2020 is in response to an open letter published in Harper's Magazine titled, A Letter on Justice and Open Debate. Using your cursor, feel free to explore peaks and valleys in the discourse around cancel culture."
  })
}

# Run the app
shinyApp(ui = ui, server = server)