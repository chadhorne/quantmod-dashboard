# Required packages
require(shinydashboard)
require(quantmod)
require(lubridate)


ui <- dashboardPage(
  
  # Header------------------------------------
  dashboardHeader(title = "Quantmod"),
  
  # Sidebar------------------------------------
  dashboardSidebar(
    
    textInput("symbol", "Symbol", value = "GOOG", width = "40%"),
    
    actionButton("getData", "Get Data!"),
    
    # Set default range for the last year
    dateInput("startDate", "Start Date", width = "55%",
              value = Sys.Date() - years(1), format = "mm/dd/yyyy"),
    
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      
      menuItem("Symbol Lookup", icon = icon("yahoo"),
               href = "https://finance.yahoo.com/lookup/")
    )
  ),
  
  # Body--------------------------------
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        
        fluidRow(
          box(title = "Stock Data", width = 8, status = "primary",
              
              plotOutput("stockPlot")
              ),
          
          box(title = "Log Differences", width = 4, status = "primary",
              
              plotOutput("logDiffPlot"))
          )
        )
      )
    )
  )

# Server--------------------------------------
server <- function(input, output) {
  
  # Get stock data
  stockData <- reactive({
    
    # Depends on getData button
    input$getData
    
    # Pull an extra year of data for analysis
    getSymbols(isolate(input$symbol), from = isolate(input$startDate - years(1)),
               auto.assign = FALSE)
  })
  
  # Plot stock data
  output$stockPlot <- renderPlot({
    
    # getData button dependency
    input$getData
    
    # Create date limit string for charting
    dateLimit <- paste0(isolate(input$startDate), "::", Sys.Date())
    
    # Chart data with analysis
    chartSeries(stockData(), subset = dateLimit, TA = c(addVo(), addBBands()))
  })
  
  output$logDiffPlot <- renderPlot({
    
    # getData button dependency
    input$getData
    
    # Create date limit string
    dateLimit <- paste0(isolate(input$startDate), "::", Sys.Date())
    
    # Compute log differences
    stockDiff <- stockData() %>% log %>% diff
    
    # Plot Close column log differences
    plot(stockDiff[, 4])
  })
}

shinyApp(ui = ui, server = server)

