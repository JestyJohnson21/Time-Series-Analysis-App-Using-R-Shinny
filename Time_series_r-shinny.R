# Load Required Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(forecast)
library(tseries)
library(readxl)
library(rugarch)
library(rmarkdown)
library(tinytex)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Time Series Analysis Tool"),
  dashboardSidebar(
    width = 400,
    fileInput("file", "Upload CSV or Excel File", accept = c(".csv", ".xlsx")),
    uiOutput("var_select"),
    numericInput("frequency", "Set Frequency of Data", value = 12, min = 1),
    actionButton("analyze", "Analyze Time Series"),
    checkboxGroupInput("additional_analysis", "Select Additional Analysis:",
                       choices = c("ACF Plot" = "acf",
                                   "PACF Plot" = "pacf",
                                   "SARIMA Model & Residual Analysis" = "sarima",
                                   "Smoothing - Moving Average" = "smoothing_ma",
                                   "Smoothing - Exponential" = "smoothing_exp",
                                   "Smoothing - Holt-Winters" = "smoothing_hw",
                                   "GARCH Model" = "garch")),
    br(),
    downloadButton("download_pdf", "Download PDF Report", class = "btn-primary")
  ),
  dashboardBody(
    fluidRow(
      column(width = 9,
             tabsetPanel(
               tabPanel("Results",
                        h3("Time Series Graph"),
                        plotOutput("ts_plot"),
                        h3("Decomposition Graph"),
                        plotOutput("trend_season"),
                        h3("Stationarity Test"),
                        verbatimTextOutput("stationarity"),
                        verbatimTextOutput("stationarity_inference"),
                        conditionalPanel(
                          condition = "input.additional_analysis.includes('acf')",
                          h3("ACF Plot"),
                          plotOutput("acf_plot")
                        ),
                        conditionalPanel(
                          condition = "input.additional_analysis.includes('pacf')",
                          h3("PACF Plot"),
                          plotOutput("pacf_plot")
                        ),
                        conditionalPanel(
                          condition = "input.additional_analysis.includes('sarima')",
                          h3("SARIMA Model Summary"),
                          verbatimTextOutput("sarima_summary"),
                          h3("SARIMA Residuals"),
                          plotOutput("sarima_residuals"),
                          h4("Shapiro-Wilk Normality Test"),
                          verbatimTextOutput("shapiro_test"),
                          h4("Ljung-Box Test for Autocorrelation"),
                          verbatimTextOutput("ljung_box_test")
                        ),
                        conditionalPanel(
                          condition = "input.additional_analysis.includes('smoothing_ma')",
                          h3("Moving Average Smoothing"),
                          plotOutput("smooth_ma")
                        ),
                        conditionalPanel(
                          condition = "input.additional_analysis.includes('smoothing_exp')",
                          h3("Exponential Smoothing"),
                          plotOutput("smooth_exp")
                        ),
                        conditionalPanel(
                          condition = "input.additional_analysis.includes('smoothing_hw')",
                          h3("Holt-Winters Smoothing"),
                          plotOutput("smooth_hw")
                        ),
                        conditionalPanel(
                          condition = "input.additional_analysis.includes('garch')",
                          h3("GARCH Model Summary"),
                          verbatimTextOutput("garch_summary"),
                          h3("GARCH Forecast (Next 10 Days)"),
                          verbatimTextOutput("garch_forecast"),
                          h3("Forecasted Volatility Plot"),
                          plotOutput("garch_volatility_plot"),
                          h3("GARCH Diagnostics"),
                          verbatimTextOutput("garch_diagnostics")
                        )
               )
             )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else if (ext == "xlsx") {
      read_excel(input$file$datapath)
    }
  })
  
  output$var_select <- renderUI({
    req(data())
    selectInput("ts_var", "Select Time Series Variable", choices = names(data()))
  })
  
  ts_data <- eventReactive(input$analyze, {
    req(input$ts_var)
    ts(data()[[input$ts_var]], frequency = input$frequency)
  })
  
  output$ts_plot <- renderPlot({
    req(ts_data())
    autoplot(ts_data()) + ggtitle("Time Series Plot")
  })
  
  output$trend_season <- renderPlot({
    req(ts_data())
    decomposed <- decompose(ts_data())
    autoplot(decomposed)
  })
  
  output$stationarity <- renderPrint({
    req(ts_data())
    adf.test(ts_data())
  })
  
  output$stationarity_inference <- renderPrint({
    req(ts_data())
    adf <- adf.test(ts_data())
    if (adf$p.value < 0.05) {
      cat("The data is stationary.")
    } else {
      cat("The data is NOT stationary.")
    }
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("ts_analysis_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      ts_series <- ts_data()
      adf_result <- adf.test(ts_series)
      stationary_text <- ifelse(adf_result$p.value < 0.05, "Data is Stationary", "Data is NOT Stationary")
      selected_analysis <- input$additional_analysis
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = list(
                          ts_data = ts_series,
                          adf_result = adf_result,
                          stationary_text = stationary_text,
                          selected_analysis = selected_analysis
                        ),
                        envir = new.env(parent = globalenv()))
    }
  )
}

# Run the app
shinyApp(ui, server)