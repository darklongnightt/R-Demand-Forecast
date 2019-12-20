library(shiny)
library(shinydashboard)
library(shinycssloaders)

dashboardPage(
  dashboardHeader(title = "Product Demand Forecast", disable = TRUE),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(
    tags$style(HTML(".main-sidebar li a { font-size: 20px; }")),
    menuItem("Demand Forecast", tabName = "Demand Forecast", icon = icon("dashboard")),
    sliderInput("month_forecast", "Months To Forecast", 2, 24, 12),
    uiOutput("productSelector"),
    actionButton("exportButton", "Forecast Product Demand", width = "88%"),
    actionButton("exportAll", "Forecast Storewide Demand", width = "88%")
  ),
  dashboardBody(
    tabsetPanel(id="tab", 
                tabPanel("FB Prophet Automatic Forecast", 
                         fluidRow(
                           withSpinner(plotOutput("prophet_plot")),
                           br(),
                           DT::dataTableOutput("sales_table_prophet")
                         )
                ),
                tabPanel("ARIMA Forecast", 
                         fluidRow(
                           withSpinner(plotOutput("arima_plot")),
                           br(),
                           DT::dataTableOutput("sales_table_arima")
                         )
                ),
                tabPanel("Holt-Winters Forecast (Additive)", 
                         fluidRow(
                           withSpinner(plotOutput("hw_additive_plot")),
                           br(),
                           DT::dataTableOutput("sales_table_hw_additive")
                         )),
                tabPanel("Holt-Winters Forecast (Multiplicative)",
                         fluidRow(
                           withSpinner(plotOutput("hw_multiplcative_plot")),
                           br(),
                           DT::dataTableOutput("sales_table_hw_multiplcative")
                         )
                ),
                tabPanel("Holt-Winters Forecast (Damped)",
                         fluidRow(
                           withSpinner(plotOutput("hw_dumped_plot")),
                           br(),
                           DT::dataTableOutput("sales_table_hw_dumped")
                         )
                )
    )
  )
)