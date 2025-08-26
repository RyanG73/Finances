# UI
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
# UI

ui <- fluidPage(
  titlePanel("Budget Spending Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectInput("name", "Select Person:", 
                  choices = unique(raw$name), selected = unique(raw$name)[1]),
      selectInput("category", "Select Spending Category:", 
                  choices = sort(unique(raw$type)), selected = sort(unique(raw$type))[1])
    ),
    mainPanel(
      # Header Row for Months
      fluidRow(
        column(4, h4(strong(textOutput("month3Header")), align = "center")),
        column(4, h4(strong(textOutput("month2Header")), align = "center")),
        column(4, h4(strong(textOutput("month1Header")), align = "center"))
      ),
      # Monthly Income Row
      fluidRow(
        column(4, valueBoxOutput("incomeMonth3")),
        column(4, valueBoxOutput("incomeMonth2")),
        column(4, valueBoxOutput("incomeMonth1"))
      ),
      # Monthly Spending Row
      fluidRow(
        column(4, valueBoxOutput("spendingMonth3")),
        column(4, valueBoxOutput("spendingMonth2")),
        column(4, valueBoxOutput("spendingMonth1"))
      ),
      # Monthly Balance Row
      fluidRow(
        column(4, valueBoxOutput("balanceMonth3")),
        column(4, valueBoxOutput("balanceMonth2")),
        column(4, valueBoxOutput("balanceMonth1"))
      ),
      # Most Recent 3 Monthly Spending Totals for Selected Category and Name
      fluidRow(
        column(4, valueBoxOutput("categoryMonth3Box", width = NULL)),
        column(4, valueBoxOutput("categoryMonth2Box", width = NULL)),
        column(4, valueBoxOutput("categoryMonth1Box", width = NULL))
      ),
      plotOutput("spendingPlot")
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
Updates:
  Single Header Row: Each column now has a single header at the top displaying the month, reducing redundancy.
Color-Coded Bottom Row: The boxes in the bottom row (for category spending) are now in purple, matching the color of the line in the chart for better visual association.
This should enhance readability and make the data easier to interpret at a glance. Let me know how it looks!
  
  
  
  
  
  
  You said:
  can you alphabetize the spending category in the dropdown?
  ChatGPT said:
  ChatGPT
Certainly! To alphabetize the spending categories in the dropdown, we can sort the choices in the selectInput for category. Here’s the small adjustment:
  
  r
Copy code
# UI
ui <- fluidPage(
  titlePanel("Budget Spending Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectInput("name", "Select Person:", 
                  choices = unique(raw$name), selected = unique(raw$name)[1]),
      selectInput("category", "Select Spending Category:", 
                  choices = sort(unique(raw$type)), selected = sort(unique(raw$type))[1])
    ),
    mainPanel(
      # Header Row for Months
      fluidRow(
        column(4, h4(strong(textOutput("month3Header")), align = "center")),
        column(4, h4(strong(textOutput("month2Header")), align = "center")),
        column(4, h4(strong(textOutput("month1Header")), align = "center"))
      ),
      # Monthly Income Row
      fluidRow(
        column(4, valueBoxOutput("incomeMonth3")),
        column(4, valueBoxOutput("incomeMonth2")),
        column(4, valueBoxOutput("incomeMonth1"))
      ),
      # Monthly Spending Row
      fluidRow(
        column(4, valueBoxOutput("spendingMonth3")),
        column(4, valueBoxOutput("spendingMonth2")),
        column(4, valueBoxOutput("spendingMonth1"))
      ),
      # Monthly Balance Row
      fluidRow(
        column(4, valueBoxOutput("balanceMonth3")),
        column(4, valueBoxOutput("balanceMonth2")),
        column(4, valueBoxOutput("balanceMonth1"))
      ),
      # Most Recent 3 Monthly Spending Totals for Selected Category and Name
      fluidRow(
        column(4, valueBoxOutput("categoryMonth3Box", width = NULL)),
        column(4, valueBoxOutput("categoryMonth2Box", width = NULL)),
        column(4, valueBoxOutput("categoryMonth1Box", width = NULL))
      ),
      plotOutput("spendingPlot")
    )
  )
)

# Server remains the same

# Run the app
shinyApp(ui = ui, server = server)

