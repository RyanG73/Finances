library(tidyverse)

setwd("/Users/ryangerda/Development/Finances")
raw <- readxl::read_excel("Data/Joint Spending Plan 2024-08.xlsx",sheet = 'raw spending') %>% 
  janitor::clean_names()
log <- readxl::read_excel("Data/Joint Spending Plan 2024-08.xlsx",sheet = 'month log') %>% 
  janitor::clean_names()

today <- as.Date('2024-10-31')#lubridate::today()
first_day_this_month <- lubridate::floor_date(today,unit='month')
last_day_this_month <- lubridate::ceiling_date(today,unit='month') - 1
first_day_this_year <- lubridate::floor_date(today,unit='year')
last_day_this_year <- lubridate::ceiling_date(today,unit='year') - 1
last_day_last_month <- first_day_this_month - 1
first_day_last_month <- lubridate::floor_date(last_day_last_month,unit='month')
first_day_next_month <- last_day_this_month + 1
last_day_next_month <-  lubridate::ceiling_date(first_day_next_month,unit='month') - 1

missing_bills <- 0

# UI
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Adding a theme to improve aesthetics
  dashboardPage(
    dashboardHeader(title = "Budget Spending Tracker", titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Settings", icon = icon("cog"))
      ),
      selectInput("name", "Select Person:", 
                  choices = unique(raw$name), selected = unique(raw$name)[1]),
      selectInput("category", "Select Spending Category:", 
                  choices = sort(unique(raw$type)), selected = sort(unique(raw$type))[1])
    ),
    dashboardBody(
      fluidRow(
        # Headers for Monthly Data
        column(4, h4(strong(textOutput("month3Header")), align = "center")),
        column(4, h4(strong(textOutput("month2Header")), align = "center")),
        column(4, h4(strong(textOutput("month1Header")), align = "center"))
      ),
      fluidRow(
        # Monthly Income Boxes with light to dark green styling
        column(4, valueBoxOutput("incomeMonth3", width = 12)),
        column(4, valueBoxOutput("incomeMonth2", width = 12)),
        column(4, valueBoxOutput("incomeMonth1", width = 12))
      ),
      fluidRow(
        # Monthly Spending Boxes with light to dark green styling
        column(4, valueBoxOutput("spendingMonth3", width = 12)),
        column(4, valueBoxOutput("spendingMonth2", width = 12)),
        column(4, valueBoxOutput("spendingMonth1", width = 12))
      ),
      fluidRow(
        # Monthly Balance Boxes with light to dark green styling
        column(4, valueBoxOutput("balanceMonth3", width = 12)),
        column(4, valueBoxOutput("balanceMonth2", width = 12)),
        column(4, valueBoxOutput("balanceMonth1", width = 12))
      ),
      fluidRow(
        # Monthly Spending by Category with light to dark green styling
        column(4, valueBoxOutput("categoryMonth3Box", width = 12)),
        column(4, valueBoxOutput("categoryMonth2Box", width = 12)),
        column(4, valueBoxOutput("categoryMonth1Box", width = 12))
      ),
      fluidRow(
        # Spending Plot
        box(title = "Monthly Spending Trend", status = "success", solidHeader = TRUE, width = 12,
            plotOutput("spendingPlot", height = "300px"))
      )
    )
  )
)
# Server
server <- function(input, output) {
  
  # Generate headers for the most recent three months
  recent_months_summary <- reactive({
    raw %>%
      mutate(month = floor_date(date, "month")) %>%
      filter(name == input$name) %>%
      group_by(month, in_out) %>%
      summarize(total = sum(amount), .groups = "drop") %>%
      pivot_wider(names_from = in_out, values_from = total, values_fill = list(total = 0)) %>%
      arrange(desc(month)) %>%
      slice(1:3)
  })
  
  output$month1Header <- renderText({
    recent_months_summary() %>% 
      slice(1) %>%
      pull(month) %>% 
      format("%B %Y")
  })
  
  output$month2Header <- renderText({
    recent_months_summary() %>% 
      slice(2) %>%
      pull(month) %>% 
      format("%B %Y")
  })
  
  output$month3Header <- renderText({
    recent_months_summary() %>% 
      slice(3) %>%
      pull(month) %>% 
      format("%B %Y")
  })
  
  # Income, Spending, Balance Boxes for each month
  for (i in 1:3) {
    local({
      month_index <- i
      output[[paste0("incomeMonth", month_index)]] <- renderValueBox({
        month_data <- recent_months_summary()
        if (nrow(month_data) >= month_index) {
          valueBox(format(month_data$In[month_index], big.mark = ","),
                   "Income", color = "green")
        } else {
          valueBox("N/A", "Income", color = "green")
        }
      })
      
      output[[paste0("spendingMonth", month_index)]] <- renderValueBox({
        month_data <- recent_months_summary()
        if (nrow(month_data) >= month_index) {
          valueBox(format(month_data$Out[month_index], big.mark = ","),
                   "Spending", color = "red")
        } else {
          valueBox("N/A", "Spending", color = "red")
        }
      })
      
      output[[paste0("balanceMonth", month_index)]] <- renderValueBox({
        month_data <- recent_months_summary()
        if (nrow(month_data) >= month_index) {
          balance <- month_data$In[month_index] - month_data$Out[month_index]
          valueBox(format(balance, big.mark = ","),
                   "Balance", color = "blue")
        } else {
          valueBox("N/A", "Balance", color = "blue")
        }
      })
    })
  }
  
  # Most Recent 3 Monthly Spending Totals for Selected Category and Name
  recent_category_spending <- reactive({
    raw %>%
      filter(type == input$category, in_out == "Out", name == input$name) %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>%
      summarize(total_spending = sum(amount), .groups = "drop") %>%
      arrange(desc(month)) %>%
      slice(1:3)
  })
  
  output$categoryMonth1Box <- renderValueBox({
    month_data <- recent_category_spending()
    if (nrow(month_data) >= 1) {
      valueBox(format(month_data$total_spending[1], big.mark = ","), 
               "Category Spending", color = "purple")
    } else {
      valueBox("N/A", "Category Spending", color = "purple")
    }
  })
  
  output$categoryMonth2Box <- renderValueBox({
    month_data <- recent_category_spending()
    if (nrow(month_data) >= 2) {
      valueBox(format(month_data$total_spending[2], big.mark = ","), 
               "Category Spending", color = "purple")
    } else {
      valueBox("N/A", "Category Spending", color = "purple")
    }
  })
  
  output$categoryMonth3Box <- renderValueBox({
    month_data <- recent_category_spending()
    if (nrow(month_data) >= 3) {
      valueBox(format(month_data$total_spending[3], big.mark = ","), 
               "Category Spending", color = "purple")
    } else {
      valueBox("N/A", "Category Spending", color = "purple")
    }
  })
  
  # Filtered spending plot by category and name
  output$spendingPlot <- renderPlot({
    filtered_data <- raw %>%
      filter(type == input$category, in_out == "Out", name == input$name) %>%
      group_by(date) %>%
      summarize(total_spending = sum(amount), .groups = "drop")
    
    ggplot(filtered_data, aes(x = date, y = total_spending)) +
      geom_line(color = "purple") +
      labs(title = paste("Spending Over Time:", input$category, "for", input$name),
           x = "Date", y = "Amount") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
