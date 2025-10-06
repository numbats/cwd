library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# Placeholder toy data: one week of hourly data so the app can run
toy_data <- tibble(
  datetime = seq.POSIXt(from = as.POSIXct('2025-01-01 00:00'), by = 'hour', length.out = 24*7),
  energy_kwh = pmax(0, 5 * sin((1: (24*7))/24 * 2 * pi) + rnorm(24*7, 0, 0.5))
)

ui <- fluidPage(
  titlePanel("Advanced Shiny: Solar dashboard (placeholder dataset)"),

  sidebarLayout(
    sidebarPanel(
      fileInput("solar_file", "Upload solar data (CSV)", accept = c('.csv')),
      dateRangeInput("date_range", "Subset dates", start = NULL, end = NULL),
      checkboxInput("show_debug", "Show debugging info", value = FALSE),
      actionButton("toggle_extra_ui", "Toggle extra UI"),
      helpText("Expected columns: datetime, energy_kwh (hourly)")
    ),

    mainPanel(
      uiOutput("maybe_debug"),
      tabsetPanel(
        id = "main_tabs",
        tabPanel("True data", br(), plotOutput("plot_true"), htmlOutput("summary_true")),
        tabPanel("Sell solar", br(), plotOutput("plot_sell"), htmlOutput("summary_sell")),
        tabPanel("Don't sell", br(), plotOutput("plot_against"), htmlOutput("summary_against"))
      )
    )
  )
)

server <- function(input, output, session) {
  solar_data <- reactive({
    if (is.null(input$solar_file)) {
      message("Using toy placeholder data. Replace by uploading real solar CSV.")
      return(toy_data)
    }
    df <- tryCatch(read.csv(input$solar_file$datapath, stringsAsFactors = FALSE),
                   error = function(e) { showNotification("Error reading file", type = "error"); NULL })
    req(df)
    if (!all(c('datetime', 'energy_kwh') %in% names(df))) { showNotification("Uploaded file must contain 'datetime' and 'energy_kwh' columns", type = "error"); return(NULL) }
    df <- df %>% mutate(datetime = lubridate::ymd_hms(datetime, quiet = TRUE), energy_kwh = as.numeric(energy_kwh))
    if (any(is.na(df$datetime)) || all(is.na(df$energy_kwh))) { showNotification("Failed to parse datetime or energy_kwh. Check formats.", type = "warning"); return(NULL) }
    df
  })

  filtered_data <- reactive({
    df <- solar_data(); req(df)
    if (!is.null(input$date_range[1]) && !is.null(input$date_range[2])) {
      df <- df %>% filter(datetime >= as.POSIXct(input$date_range[1]) & datetime <= as.POSIXct(input$date_range[2]))
    }
    df
  })

  daily_aggr <- reactive({
    df <- filtered_data(); req(df)
    df %>% mutate(date = as.Date(datetime)) %>% group_by(date) %>% summarise(total_kwh = sum(energy_kwh, na.rm = TRUE), mean_kwh = mean(energy_kwh, na.rm = TRUE)) %>% ungroup()
  })

  output$maybe_debug <- renderUI({ if (!isTRUE(input$show_debug)) return(NULL); tagList(wellPanel(h4("Debugging info"), verbatimTextOutput("debug_text"))) })
  output$debug_text <- renderPrint({ list(file_input = input$solar_file, nrows = if (!is.null(solar_data())) nrow(solar_data()) else NA, tabs = input$main_tabs, date_range = input$date_range) })

  summary_text <- reactive({ df <- filtered_data(); if (is.null(df)) return("No data available. Upload a CSV with hourly energy to see summaries."); days <- length(unique(as.Date(df$datetime))); total <- sum(df$energy_kwh, na.rm = TRUE); peak <- max(df$energy_kwh, na.rm = TRUE); paste0("Data summary: ", days, " days (", round(days/30,2), " months approx.), total energy = ", round(total,1), " kWh, peak hourly = ", round(peak,2), " kWh.") })

  output$summary_true <- renderUI({ HTML(paste0("<b>True data summary:</b><br>", summary_text())) })

  output$summary_sell <- renderUI({ df <- filtered_data(); if (is.null(df)) return(HTML("<b>Sell solar summary:</b><br>No data")); total <- sum(df$energy_kwh, na.rm = TRUE); est_savings = total * 0.25; HTML(paste0("<b>Sell solar summary:</b><br>", "Total energy = ", round(total,1), " kWh. ", "Estimated household savings: $", round(est_savings,0), " per period (placeholder).")) })

  output$summary_against <- renderUI({ df <- filtered_data(); if (is.null(df)) return(HTML("<b>Don't sell summary:</b><br>No data")); variability <- sd(df$energy_kwh, na.rm = TRUE); HTML(paste0("<b>Don't sell summary:</b><br>", "High hourly variability (sd = ", round(variability,2), ") may reduce reliability. Consider local demand patterns.")) })

  output$plot_true <- renderPlot({ df <- filtered_data(); req(df); ggplot(df, aes(x = datetime, y = energy_kwh)) + geom_line(colour = 'steelblue') + labs(title = 'Hourly energy (true)', y = 'kWh', x = '') + theme_minimal() })

  output$plot_sell <- renderPlot({ df <- daily_aggr(); req(df); df %>% mutate(cum = cumsum(total_kwh)) %>% ggplot(aes(x = date, y = cum)) + geom_line(colour = 'darkgreen', size = 1.2) + labs(title = 'Cumulative energy (sell framing)', y = 'Cumulative kWh', x = '') + theme_minimal() })

  output$plot_against <- renderPlot({ df <- filtered_data(); req(df); df %>% ggplot(aes(x = datetime, y = energy_kwh)) + geom_point(aes(colour = abs(energy_kwh - zoo::rollmean(energy_kwh, 24, fill = NA))), alpha = 0.6) + labs(title = "Hourly energy (against framing - shows variability)", y = 'kWh', x = '') + theme_minimal() })

  observeEvent(input$toggle_extra_ui, { current <- isolate(session$userData$extra_ui); session$userData$extra_ui <- is.null(current) || !current })
  outputOptions(output, "maybe_debug", suspendWhenHidden = FALSE)
  observe({ if (isTRUE(session$userData$extra_ui)) { insertUI(selector = "#toggle_extra_ui", where = "afterEnd", ui = tags$div(id = "extra_controls", selectInput("fake_opt", "Extra option (demo)", choices = c('A','B'), selected = 'A'))) } else { removeUI(selector = "#extra_controls") } })
}

shinyApp(ui = ui, server = server)
