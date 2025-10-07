library(shiny)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(lubridate)

add_solar_model <- function(solar_data, max_kw = 5) {

  hours <- lubridate::hour(solar_data$datetime)

  production <- ifelse(
    hours >= 8 & hours <= 18,
    max_kw/2 * sin(pi * (hours - 6) / 12)^1.5 + rnorm(length(hours), mean = 0, sd = max_kw * 0.02),
    0
  )

  # Ensure no negative values due to noise
  production <- pmax(0, production)

  solar_data |> mutate(production = production)
}

ui <- fluidPage(
  headerPanel("Solar dashboard"),
  sidebarPanel(
    fileInput("solar_file", "Upload solar data CSV", accept = c(".csv")),
    sliderInput("date_range", "Subset dates", min = as.Date('2024-07-15'), max = as.Date('2025-10-05'),
                value = c(as.Date('2024-07-15'), as.Date('2025-10-05'))),
    numericInput("solar_size", "Solar System Size", value = 5, min = 1, max = 20),
    div(id = "price_inputs",
        numericInput("electricity_price_1", "Price per kwh", value = 0.28, min = 0)
    ),
    div(style = "text-align: right;",
      actionButton("add_price", "Add another price"),
  ),

    radioButtons("plot_geom", "Plot type",
                 choices = c("Line" = "line", "Histogram" = "hist"),
                 selected = "line"),
    conditionalPanel(
      condition = "input.plot_geom == 'hist'",
      sliderInput("bins", "Number of bins", min = 5, max = 60, value = 30),
    )

  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Plot", plotOutput("solar_plot"), downloadButton("download_plot", "Download Plot")),
      tabPanel("Cost Plot", plotOutput("cost_plot"), downloadButton("download_cost_plot", "Download Cost Plot")),
      tabPanel("Production Plot", plotOutput("production_plot"), textOutput("energy_wasted")),
      tabPanel("Data", DTOutput("data_table"))
    )
  )
)

server <- function(input, output) {

  n_prices <- reactiveVal(1)
  observeEvent(input$add_price, {
    n <- n_prices() + 1
    n_prices(n)

    insertUI(
      selector = "#price_inputs",
      where = "beforeEnd",
      ui = numericInput(
        paste0("electricity_price_", n),
        paste("Price per kwh"), value = 0.28, min = 0)
    )
  })

  solar_data <- reactive({
    req(input$solar_file)
    read.csv(input$solar_file$datapath) |>
    pivot_longer(
    cols = starts_with("X"),
    names_to = "datetime",
    values_to = "energy_kwh"
  ) |>
  mutate(
    datetime = gsub("X(\\d{2}\\.\\d{2})\\.\\.\\.(\\d{2}\\.\\d{2})", "\\1", datetime),
    energy_kwh = as.numeric(energy_kwh),
    datetime = paste0(DATE, " ", datetime),
    datetime = lubridate::ymd_hm(datetime)
  ) |>
  janitor::clean_names()
  })

  solar_data_filtered <- reactive({
    req(solar_data())
    solar_data() |>
      filter(datetime >= input$date_range[1] & datetime <= input$date_range[2]) |>
      add_solar_model(max_kw = input$solar_size) |>
      mutate(net_use = energy_kwh - production)
  })

  solar_data_priced <- reactive({
    req(prices())
    solar_data_priced <- solar_data_filtered()
    for (i in 1:n_prices()) {
      solar_data_priced[, paste0("electricity_net_", i)] <- prices()[i] * pmax(solar_data_priced$net_use, 0)
      solar_data_priced[, paste0("electricity_price_", i)] <- prices()[i] * pmax(solar_data_priced$energy_kwh, 0)
      solar_data_priced[, paste0("total_cost_", i)] <- cumsum(solar_data_priced[, paste0("electricity_price_", i)])
      solar_data_priced[, paste0("total_net_", i)] <- cumsum(solar_data_priced[, paste0("electricity_net_", i)])
    }

    solar_data_priced
  })

  solar_plot <- reactive({
    plot <- ggplot(solar_data_priced()) +
          labs(y = 'kWh', x = 'Date')
        if (input$plot_geom == "line") {
          ggplot(solar_data_priced()) + geom_line(aes(x=datetime, y=energy_kwh)) +
            labs(y='kWh', x='Date')
        } else if (input$plot_geom == "hist") {
          ggplot(solar_data_priced()) + geom_histogram(bins = input$bins, aes(x = energy_kwh)) +
            labs(y='Count', x='Energy (kWh)')
    }
  })

  output$solar_plot <- renderPlot({
    solar_plot()
  })


  output$download_plot <- downloadHandler(
    filename = function() { paste("solar_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
          ggsave(file, plot = solar_plot(), device = "png", bg = "white", width = 8, height = 6)
    }
  )

  output$download_cost_plot <- downloadHandler(
    filename = function() { paste("cost_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
          ggsave(file, plot = cost_plot(), device = "png", bg = "white", width = 8, height = 6)
    }
  )

  output$data_table <- renderDT({
    req(solar_data())
    solar_data_filtered()
  })


  prices <- reactive({
    ids <- paste0("electricity_price_", 1:n_prices())
    values <- sapply(ids, function(id) input[[id]])
    values[!is.na(values)]
  })


  cost_plot <- reactive({
    req(solar_data_priced())
    req(prices())
    solar_data_priced() |>
      select(datetime, starts_with("total_")) |>
      pivot_longer(!c(datetime), names_to = c("cost_type", "cost_per_kwh"), names_pattern = "total_(cost|net)_(\\d+)", values_to = "cost") |>
      mutate(
        cost_per_kwh = factor(cost_per_kwh, labels = scales::dollar(prices())),
        cost_type = factor(cost_type, labels = c("Without solar", "With Solar"))
      ) |>
    ggplot(aes(x = datetime, y = cost, colour = cost_per_kwh, linetype = cost_type)) +
      geom_line() +
      labs(y = 'Cost ($)', x = 'Date', colour = "Cost/kwh", linetype = "") +
      theme_minimal()
  })
  output$cost_plot <- renderPlot({
    cost_plot()
  })

  production_plot <- reactive({
    req(solar_data_priced())

    solar_data_priced() |>
      ggplot(aes(x=datetime, y=production)) + geom_line()
  })

  output$production_plot <- renderPlot({
    production_plot()
  })

  output$energy_wasted <- renderText({
    energy_not_used <- solar_data_priced() |>
      filter(net_use < 0) |>
      summarise(total_wasted = -1*sum(net_use)) |>
      pull(total_wasted) |>
      round() |>
      scales::comma()
    paste0("For a system of this size, ", energy_not_used, "kWh are being wasted.")
  })

}

shinyApp(ui, server)
