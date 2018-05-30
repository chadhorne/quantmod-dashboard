# Required packages
require(shiny)
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
          box(title = "Stock Data", width = 8,
              
              plotOutput("stockPlot")
              )
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
    
    getSymbols(isolate(input$symbol), src = "yahoo", from = isolate(input$startDate), auto.assign = FALSE)
  })
  
  # Plot stock data
  output$stockPlot <- renderPlot({
    
    chartSeries(stockData())
    
    addSMA(n = 50)
  })
}

shinyApp(ui = ui, server = server)

