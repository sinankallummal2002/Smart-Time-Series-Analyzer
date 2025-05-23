library(shiny)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(FinTS)
library(shinycssloaders)  # Required for withSpinner()

# UI Definition
ui <- navbarPage(
  title = "Smart Time Series Analyzer",
  id = "main_nav",
  header = tags$head(
    tags$script(src = "https://cdn.tailwindcss.com"),
    tags$style(HTML("
      /* Main color scheme */
      :root {
        --primary: #4361ee;
        --secondary: #3a0ca3;
        --accent: #f72585;
        --light: #f8f9fa;
        --dark: #212529;
      }
      
      /* Tab styling */
      .nav-tabs {
        margin-bottom: 20px;
        background-color: var(--light);
        border-bottom: 2px solid var(--primary);
      }
      
      .nav-tabs > li > a {
        color: var(--dark);
        font-weight: 500;
        border: none !important;
        margin-right: 5px;
      }
      
      .nav-tabs > li.active > a {
        color: var(--primary) !important;
        background-color: white !important;
        border-bottom: 3px solid var(--accent) !important;
      }
      
      /* Content styling */
      .tab-content { 
        padding: 25px;
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
      }
      
      /* Plot containers */
      .plot-container { 
        margin-top: 20px;
        padding: 15px;
        background-color: white;
        border-radius: 8px;
        border-left: 4px solid var(--primary);
      }
      
      /* Buttons */
      .btn {
        transition: all 0.3s ease;
      }
      
      .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      /* Cards */
      .card {
        background-color: white;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        border-top: 3px solid var(--primary);
      }
      
      /* Headers */
      h3 {
        color: var(--secondary);
        border-bottom: 2px solid var(--light);
        padding-bottom: 8px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  # Tab 1: Data Upload
  tabPanel(
    "1. Upload Data",
    value = "upload",
    div(class = "p-4",
        fileInput("data_file", "Upload CSV File", accept = ".csv"),
        uiOutput("upload_warning"),
        uiOutput("column_selector"),
        tableOutput("data_preview"),
        actionButton("upload_next", "Next: Stationarity Check", 
                     class = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded")
    )
  ),
  
  # Tab 2: Stationarity Check
  tabPanel(
    "2. Stationarity Check",
    value = "stationarity",
    div(class = "p-4",
        plotOutput("ts_plot"),
        actionButton("run_adf", "Run ADF Test"),
        actionButton("run_kpss", "Run KPSS Test"),
        verbatimTextOutput("stationarity_check"),
        actionButton("stationarity_next", "Next: Differencing", 
                     class = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded")
    )
  ),
  
  # Tab 3: Differencing
  tabPanel(
    "3. Differencing",
    value = "differencing",
    div(class = "p-4",
        numericInput("d_order", "Differencing Order (d):", value = 0, min = 0),
        numericInput("D_order", "Seasonal Differencing (D):", value = 0, min = 0),
        numericInput("seasonal_period", "Seasonal Period:", value = 1, min = 1),
        actionButton("detect_seasonality", "Auto-Detect Seasonality"),
        plotOutput("diff_plot"),
        verbatimTextOutput("diff_summary"),
        actionButton("differencing_next", "Next: Smoothing", 
                     class = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded")
    )
  ),
  
  # Tab 4: Smoothing
  tabPanel(
    "4. Smoothing",
    value = "smoothing",
    div(class = "p-4",
        radioButtons("smoothing_method", "Smoothing Method:",
                     choices = c("Moving Average", "Exponential Smoothing", "Holt-Winters")),
        uiOutput("smoothing_params"),
        plotOutput("smooth_plot"),
        verbatimTextOutput("smooth_summary"),
        actionButton("smoothing_next", "Next: Model Suggestion", 
                     class = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded")
    )
  ),
  
  # Tab 5: Model Selection & Forecasting
  tabPanel(
    "5. Model & Forecasting",
    value = "model",
    div(class = "p-4",
        h3("Model Selection"),
        radioButtons("model_type", "Model Type:",
                     choices = c("ARIMA", "SARIMA"),
                     selected = "ARIMA"),
        actionButton("fit_model", "Fit Selected Model",
                     class = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"),
        
        h3("Model Summary"),
        verbatimTextOutput("model_summary"),
        
        h3("Forecasting Results"),
        plotOutput("forecast_plot") %>% withSpinner(),
        verbatimTextOutput("forecast_metrics"),
        
        h3("Residual Diagnostics"),
        plotOutput("residual_plots") %>% withSpinner(),
        
        actionButton("model_next", "Generate Final Report", 
                     class = "bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded")
    )
  ),
  
  # Tab 6: GARCH Modeling
  tabPanel(
    "6. GARCH Modeling",
    value = "garch_check",
    div(class = "p-4",
        h3("Volatility Analysis"),
        plotOutput("volatility_plot") %>% withSpinner(),
        verbatimTextOutput("volatility_check"),
        
        h3("GARCH Model"),
        numericInput("garch_p", "GARCH order (p):", value = 1, min = 0),
        numericInput("garch_q", "ARCH order (q):", value = 1, min = 0),
        actionButton("run_garch", "Fit GARCH Model",
                     class = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"),
        
        h3("GARCH Results"),
        verbatimTextOutput("garch_summary"),
        plotOutput("garch_forecast_plot") %>% withSpinner(),
        
        actionButton("to_report", "Proceed to Report", 
                     class = "bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded")
    )
  ),
  
  # Tab 7: Final Report
  tabPanel(
    "7. Final Report",
    value = "report",
    div(class = "p-4",
        h3("Time Series Analysis Report"),
        div(class = "bg-gray-100 p-4 rounded-lg",
            p("A comprehensive PDF report with all analysis results is ready for download."),
            p("The report includes:"),
            tags$ul(
              tags$li("Time series decomposition"),
              tags$li("Stationarity test results"),
              tags$li("ACF/PACF analysis"),
              tags$li("Model selection and diagnostics"),
              tags$li("Forecasting results"),
              tags$li("Volatility analysis")
            )
        ),
        downloadButton("download_report", "Download PDF Report",
                       class = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"),
        actionButton("restart", "Start New Analysis", 
                     class = "bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  output$upload_warning <- renderUI({
    if (is.null(rv$ts_data)) {
      div(style = "color: #d9534f; background-color: #f2dede; 
                border: 1px solid #ebccd1; padding: 10px; 
                border-radius: 4px; margin-top: 5px;",
          "⚠️ Please upload your time series dataset to proceed.")
    }
  })
  # Reactive values to store data and state
  rv <- reactiveValues(
    data = NULL,
    ts_data = NULL,
    stationary = FALSE,
    diff_data = NULL,
    smooth_data = NULL,
    model = NULL
  )
  
  # Data Upload Tab
  observeEvent(input$data_file, {
    req(input$data_file)
    rv$data <- read.csv(input$data_file$datapath)
    output$data_preview <- renderTable(head(rv$data, 10))
    
    # Update column selector
    output$column_selector <- renderUI({
      selectInput("ts_column", "Select Time Series Column:", 
                  choices = names(rv$data))
    })
  })
  
  observeEvent(input$upload_next, {
    req(input$ts_column)
    rv$ts_data <- ts(rv$data[[input$ts_column]])
    updateNavbarPage(session, "main_nav", selected = "stationarity")
  })
  
  # Stationarity Tab
  output$ts_plot <- renderPlot({
    req(rv$ts_data)
    autoplot(rv$ts_data) + 
      labs(title = "Original Time Series", y = "Value") +
      theme_minimal()
  })
  
  output$decomp_plot <- renderPlot({
    req(rv$ts_data)
    decomp <- stl(rv$ts_data, s.window = "periodic")
    autoplot(decomp) + 
      labs(title = "Time Series Decomposition") +
      theme_minimal()
  })
  
  output$stationarity_check <- renderPrint({
    req(rv$ts_data)
    tryCatch({
      adf <- tseries::adf.test(rv$ts_data)
      kpss <- tseries::kpss.test(rv$ts_data)
      
      cat("=== STATIONARITY TEST RESULTS ===\n\n")
      cat("ADF Test (p-value):", format.pval(adf$p.value, digits = 3), "\n")
      cat("KPSS Test (p-value):", format.pval(kpss$p.value, digits = 3), "\n\n")
      
      if(adf$p.value > 0.05 || kpss$p.value < 0.05) {
        cat("CONCLUSION: Non-Stationary - Differencing recommended\n")
        d_order <- forecast::ndiffs(rv$ts_data, test="kpss")
        cat("Suggested differencing order (d):", d_order, "\n")
        
        seasonality <- forecast::findfrequency(rv$ts_data)
        if(seasonality > 1) {
          cat("Suggested seasonal differencing (D): 1\n")
          cat("Detected seasonality period:", seasonality, "\n")
          rv$seasonal_period <- seasonality
        }
      } else {
        cat("CONCLUSION: Stationary - Ready for modeling\n")
      }
    }, error = function(e) {
      cat("Error in stationarity tests:", e$message, "\n")
    })
  })
  
  observeEvent(input$stationarity_next, {
    updateNavbarPage(session, "main_nav", selected = "differencing")
  })
  
  # Differencing Tab
  observeEvent(input$detect_seasonality, {
    req(rv$ts_data)
    period <- findfrequency(rv$ts_data)
    updateNumericInput(session, "seasonal_period", value = period)
    if(period > 1) {
      showNotification(paste("Detected seasonality with period", period), 
                       type = "message")
    }
  })
  
  output$acf_plot <- renderPlot({
    req(rv$ts_data)
    ggAcf(rv$ts_data) + 
      labs(title = "Autocorrelation Function (ACF)") +
      theme_minimal()
  })
  
  output$pacf_plot <- renderPlot({
    req(rv$ts_data)
    ggPacf(rv$ts_data) + 
      labs(title = "Partial Autocorrelation Function (PACF)") +
      theme_minimal()
  })
  
  output$diff_plot <- renderPlot({
    req(rv$ts_data)
    d <- input$d_order
    D <- input$D_order
    period <- input$seasonal_period
    
    if (d > 0 || D > 0) {
      diff_series <- rv$ts_data
      if (d > 0) diff_series <- diff(diff_series, differences = d)
      if (D > 0) diff_series <- diff(diff_series, lag = period, differences = D)
      
      rv$diff_data <- diff_series
      autoplot(diff_series) + 
        labs(title = "Differenced Series", y = "Value") +
        theme_minimal()
    } else {
      autoplot(rv$ts_data) + 
        labs(title = "Original Series (No Differencing)", y = "Value") +
        theme_minimal()
    }
  })
  
  output$diff_summary <- renderPrint({
    req(rv$diff_data)
    cat("Differencing Summary:\n")
    cat("Applied d =", input$d_order, "and D =", input$D_order, 
        "(seasonality =", input$seasonal_period, ")\n")
    cat("New series has", length(rv$diff_data), "observations\n")
  })
  
  observeEvent(input$differencing_next, {
    updateNavbarPage(session, "main_nav", selected = "smoothing")
  })
  
  # Smoothing Tab
  output$smoothing_params <- renderUI({
    req(input$smoothing_method)
    if (input$smoothing_method == "Moving Average") {
      sliderInput("ma_window", "Window Size:", min = 2, max = 12, value = 3)
    } else if (input$smoothing_method == "Exponential Smoothing") {
      sliderInput("alpha", "Alpha (Level):", min = 0, max = 1, value = 0.2)
    } else {
      tagList(
        sliderInput("alpha_hw", "Alpha (Level):", min = 0, max = 1, value = 0.2),
        sliderInput("beta_hw", "Beta (Trend):", min = 0, max = 1, value = 0.1),
        sliderInput("gamma_hw", "Gamma (Seasonality):", min = 0, max = 1, value = 0.1)
      )
    }
  })
  
  output$smooth_plot <- renderPlot({
    req(rv$ts_data)
    if (input$smoothing_method == "Moving Average") {
      smoothed <- ma(rv$ts_data, order = input$ma_window)
    } else if (input$smoothing_method == "Exponential Smoothing") {
      smoothed <- ses(rv$ts_data, alpha = input$alpha)
    } else {
      smoothed <- HoltWinters(rv$ts_data, 
                              alpha = input$alpha_hw,
                              beta = input$beta_hw,
                              gamma = input$gamma_hw)
    }
    
    rv$smooth_data <- smoothed
    autoplot(rv$ts_data, series = "Original") +
      autolayer(smoothed, series = "Smoothed") +
      labs(title = "Smoothed Series", y = "Value") +
      scale_color_manual(values = c("Original" = "black", "Smoothed" = "red"))
  })
  
  output$smooth_summary <- renderPrint({
    req(rv$smooth_data)
    cat("Smoothing Summary:\n")
    print(rv$smooth_data)
  })
  
  observeEvent(input$smoothing_next, {
    updateNavbarPage(session, "main_nav", selected = "model")
  })
  
  # Enhanced Model Identification Function
  identify_model <- function(ts_data, d, D, period) {
    # Calculate volatility ratio (SD/mean)
    volatility_ratio <- sd(ts_data, na.rm = TRUE)/mean(ts_data, na.rm = TRUE)
    
    # Check for ARCH effects
    arch_test <- tryCatch({
      FinTS::ArchTest(residuals(na.omit(ts_data)))
    }, error = function(e) {
      list(p.value = 1) # Default to no ARCH effects if test fails
    })
    
    # Always suggest GARCH if volatility ratio > 0.5
    if(volatility_ratio > 0.5) {
      return(list(type="GARCH_OPTIONAL", 
                  message=paste("High volatility detected (Ratio:", 
                                round(volatility_ratio,4), 
                                "). Consider GARCH modeling.")))
    }
    
    # Otherwise check standard conditions
    if(arch_test$p.value < 0.05 && d == 0) {
      return(list(type="GARCH", 
                  message=paste("ARCH effects detected (p-value:", 
                                round(arch_test$p.value,4), ")")))
    }
    
    # Check for seasonality
    spectral <- spectrum(ts_data, plot=FALSE)
    if(max(spectral$spec) > 3*mean(spectral$spec)) {
      return(list(type="SARIMA",
                  message=paste("Strong seasonality detected with period", period)))
    }
    
    # Default to ARIMA
    return(list(type="ARIMA",
                message="No strong seasonality or volatility detected"))
  }
  
  # Model Fitting
  observeEvent(input$fit_model, {
    req(rv$ts_data)
    tryCatch({
      withProgress(message = 'Fitting model...', value = 0.5, {
        # Split data into train/test (80/20)
        n <- length(rv$ts_data)
        train_end <- floor(0.8 * n)
        train <- window(rv$ts_data, end = train_end)
        test <- window(rv$ts_data, start = train_end + 1)
        
        # Fit selected model
        if(input$model_type == "SARIMA") {
          model <- auto.arima(train, 
                              d = input$d_order,
                              D = input$D_order,
                              seasonal = TRUE,
                              stepwise = FALSE,
                              approximation = FALSE)
        } else {
          model <- auto.arima(train,
                              d = input$d_order,
                              seasonal = FALSE,
                              stepwise = FALSE,
                              approximation = FALSE)
        }
        
        # Store model and generate forecasts
        rv$model <- model
        rv$forecast <- forecast(model, h = length(test))
        rv$test_data <- test
        
        incProgress(0.8, detail = "Generating forecasts...")
      })
    }, error = function(e) {
      showNotification(paste("Model fitting failed:", e$message), type = "error")
    })
  })
  
  output$model_summary <- renderPrint({
    req(rv$model)
    cat("=== MODEL SUMMARY ===\n\n")
    print(rv$model)
    cat("\nAIC:", rv$model$aic, "\n")
    cat("BIC:", rv$model$bic, "\n")
  })
  
  output$forecast_plot <- renderPlot({
    req(rv$forecast, rv$test_data)
    autoplot(rv$forecast) +
      autolayer(rv$test_data, series = "Actual") +
      labs(title = "Forecast vs Actual", y = "Value") +
      theme_minimal()
  })
  
  output$forecast_metrics <- renderPrint({
    req(rv$forecast, rv$test_data)
    acc <- accuracy(rv$forecast, rv$test_data)
    cat("=== FORECAST ACCURACY ===\n\n")
    print(acc)
  })
  
  output$residual_plots <- renderPlot({
    req(rv$model)
    checkresiduals(rv$model) +
      theme_minimal()
  })
  
  observeEvent(input$model_next, {
    updateNavbarPage(session, "main_nav", selected = "report")
  })
  
  # Final Report Tab
  output$full_report <- renderPrint({
    req(rv$ts_data)
    
    model_info <- identify_model(rv$ts_data, 
                                 input$d_order, 
                                 input$D_order, 
                                 input$seasonal_period)
    
    cat("=== COMPREHENSIVE TIME SERIES ANALYSIS REPORT ===\n\n")
    cat("1. DATA CHARACTERISTICS\n")
    cat("   - Observations:", length(rv$ts_data), "\n")
    cat("   - Range:", range(rv$ts_data), "\n")
    cat("   - Mean:", mean(rv$ts_data), "\n")
    cat("   - Standard Deviation:", sd(rv$ts_data), "\n")
    cat("   - Volatility (SD/Mean):", round(sd(rv$ts_data)/mean(rv$ts_data), 4), "\n\n")
    
    cat("2. STATIONARITY ANALYSIS\n")
    adf <- tseries::adf.test(rv$ts_data)
    kpss <- tseries::kpss.test(rv$ts_data)
    cat("   - ADF Test p-value:", round(adf$p.value, 4), "\n")
    cat("   - KPSS Test p-value:", round(kpss$p.value, 4), "\n")
    cat("   - Conclusion:", ifelse(adf$p.value > 0.05 || kpss$p.value < 0.05, 
                                   "Non-Stationary", "Stationary"), "\n\n")
    
    cat("3. TRANSFORMATIONS APPLIED\n")
    if (!is.null(rv$diff_data)) {
      cat("   - Regular Differencing Order (d):", input$d_order, "\n")
      cat("   - Seasonal Differencing Order (D):", input$D_order, "\n")
      cat("   - Seasonal Period:", input$seasonal_period, "\n")
    } else {
      cat("   - No differencing applied\n")
    }
    cat("   - Smoothing Method:", ifelse(!is.null(input$smoothing_method), 
                                         input$smoothing_method, "None"), "\n\n")
    
    cat("4. MODEL RECOMMENDATION\n")
    cat("   - Selected Model:", model_info$type, "\n")
    cat("   - Reasoning:", model_info$message, "\n")
    
    if(model_info$type == "GARCH") {
      cat("   - Suggested Order: GARCH(1,1)\n")
    } else if(model_info$type == "SARIMA") {
      cat("   - Suggested Form: SARIMA(p,",input$d_order,",q)(",
          "P,",input$D_order,",Q)[",input$seasonal_period,"]\n")
    } else {
      cat("   - Suggested Form: ARIMA(p,",input$d_order,",q)\n")
    }
    
    cat("\n5. DIAGNOSTIC CHECKS\n")
    if(model_info$type == "GARCH") {
      arch_test <- tryCatch({
        FinTS::ArchTest(residuals(na.omit(rv$ts_data)))
      }, error = function(e) {
        list(p.value = NA)
      })
      cat("   - ARCH Test p-value:", 
          ifelse(is.na(arch_test$p.value), "Test failed", 
                 round(arch_test$p.value, 4)), "\n")
    } else {
      cat("   - Residuals ACF/PACF: See plots\n")
    }
    
    cat("\n=== END REPORT ===\n")
  })
  
  # Downloadable PDF report
  output$download_report <- downloadHandler(
    filename = function() {
      paste("time-series-report-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      # Create temp directory for plots
      temp_dir <- tempdir()
      plot_files <- c()
      
      # Save all plots to temp files
      tryCatch({
        # Time series plot
        ts_plot <- autoplot(rv$ts_data) + labs(title = "Original Time Series")
        ts_path <- file.path(temp_dir, "ts_plot.png")
        ggsave(ts_path, ts_plot, width = 8, height = 5)
        plot_files <- c(plot_files, ts_path)
        
        # Decomposition plot
        decomp_plot <- autoplot(stl(rv$ts_data, s.window = "periodic")) + 
          labs(title = "Time Series Decomposition")
        decomp_path <- file.path(temp_dir, "decomp_plot.png")
        ggsave(decomp_path, decomp_plot, width = 8, height = 5)
        plot_files <- c(plot_files, decomp_path)
        
        # ACF/PACF plots
        acf_plot <- ggAcf(rv$ts_data) + labs(title = "ACF Plot")
        acf_path <- file.path(temp_dir, "acf_plot.png")
        ggsave(acf_path, acf_plot, width = 8, height = 5)
        plot_files <- c(plot_files, acf_path)
        
        pacf_plot <- ggPacf(rv$ts_data) + labs(title = "PACF Plot")
        pacf_path <- file.path(temp_dir, "pacf_plot.png")
        ggsave(pacf_path, pacf_plot, width = 8, height = 5)
        plot_files <- c(plot_files, pacf_path)
        
        # Forecast plot
        if(!is.null(rv$forecast)) {
          forecast_plot <- autoplot(rv$forecast) + 
            autolayer(rv$test_data, series = "Actual") +
            labs(title = "Forecast vs Actual")
          forecast_path <- file.path(temp_dir, "forecast_plot.png")
          ggsave(forecast_path, forecast_plot, width = 8, height = 5)
          plot_files <- c(plot_files, forecast_path)
        }
        
        # GARCH plot
        if(!is.null(rv$garch_forecast)) {
          garch_plot <- plot(sigma(rv$garch_forecast), 
                             main = "Volatility Forecast",
                             ylab = "Conditional SD")
          garch_path <- file.path(temp_dir, "garch_plot.png")
          ggsave(garch_path, garch_plot, width = 8, height = 5)
          plot_files <- c(plot_files, garch_path)
        }
        
        # Render the report
        rmarkdown::render(
          input = "report_template.Rmd",
          output_file = file,
          params = list(
            ts_plot_path = ts_path,
            decomp_plot_path = decomp_path,
            acf_plot_path = acf_path,
            pacf_plot_path = pacf_path,
            forecast_plot_path = if(exists("forecast_path")) forecast_path else NULL,
            volatility_plot_path = if(exists("garch_path")) garch_path else NULL,
            data_summary = paste("Series length:", length(rv$ts_data), 
                                 "Mean:", round(mean(rv$ts_data), 2),
                                 "SD:", round(sd(rv$ts_data), 2)),
            stationarity_conclusion = if(adf$p.value > 0.05 || kpss$p.value < 0.05) 
              "Non-Stationary" else "Stationary",
            model_recommendation = if(!is.null(rv$model)) 
              paste("Selected model:", rv$model$arma) else "No model selected",
            forecast_accuracy = if(!is.null(rv$forecast)) 
              paste("RMSE:", round(accuracy(rv$forecast, rv$test_data)[2], 2)) 
            else "No forecast available"
          ),
          envir = new.env(parent = globalenv())
        )
      }, finally = {
        # Clean up temp files
        unlink(plot_files)
      })
    }
  )
  
  # Quick GARCH Check Tab
  output$volatility_check <- renderPrint({
    req(rv$ts_data)
    volatility_ratio <- sd(rv$ts_data, na.rm = TRUE)/mean(rv$ts_data, na.rm = TRUE)
    cat("=== VOLATILITY ANALYSIS ===\n\n")
    cat("Volatility Ratio (SD/Mean):", round(volatility_ratio, 4), "\n")
    if(volatility_ratio > 0.5) {
      cat("\nCONCLUSION: High volatility detected - GARCH recommended\n")
    } else {
      cat("\nCONCLUSION: Moderate volatility - GARCH may not be needed\n")
    }
  })
  
  output$volatility_plot <- renderPlot({
    req(rv$ts_data)
    plot(residuals(na.omit(rv$ts_data))^2, 
         main="Squared Residuals (Volatility Clustering)",
         ylab="Squared Residuals")
  })
  
  observeEvent(input$run_garch, {
    req(rv$ts_data)
    output$garch_results <- renderPrint({
      tryCatch({
        garch_model <- garch(rv$ts_data, trace=FALSE)
        cat("=== GARCH(1,1) RESULTS ===\n\n")
        print(summary(garch_model))
      }, error = function(e) {
        cat("GARCH modeling failed:", e$message, "\n")
      })
    })
  })
  
  observeEvent(input$to_report, {
    updateNavbarPage(session, "main_nav", selected = "report")
  })
  
  # Restart analysis
  observeEvent(input$restart, {
    session$reload()
  })
}

# Run the application
shinyApp(ui = ui, server = server)