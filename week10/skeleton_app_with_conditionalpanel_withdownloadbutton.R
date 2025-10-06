library(shiny)
library(DT)

ui <- fluidPage(
  headerPanel("Solar dashboard"),
  sidebarPanel(
    fileInput("solar_file", "Upload solar data CSV", accept = c(".csv")),
    sliderInput("date_range", "Subset dates", min = as.Date('2024-07-15'), max = as.Date('2025-10-05'),
                value = c(as.Date('2024-07-15'), as.Date('2025-10-05'))),

    radioButtons("plot_geom", "Plot type",
                 choices = c("Line" = "line", "Histogram" = "hist"),
                 selected = "line"),
    conditionalPanel(
      condition = "input.plot_geom == 'hist'",
      sliderInput("bins", "Number of bins", min = 5, max = 60, value = 30),
      selectInput("aggregation_size", "Aggregation type", choices = c("None", "Daily", "Weekly", "Monthly")),
    )

  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("solar_plot"), downloadButton("download_plot", "Download Plot")),
      tabPanel("Data", DTOutput("data_table"))
    )
  )
)

server <- function(input, output) {

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
      filter(datetime >= input$date_range[1] & datetime <= input$date_range[2])
  })

  solar_data_aggregated <- reactive({
    req(solar_data_filtered())
    if (input$aggregation_size == "Daily") {
      solar_data_filtered() |>
        group_by(date) |>
        summarise(energy_kwh = sum(energy_kwh, na.rm = TRUE))
    } else if (input$aggregation_size == "Weekly") {
      solar_data_filtered() |>
        mutate(week = lubridate::floor_date(datetime, unit = "week")) |>
        group_by(week) |>
        summarise(energy_kwh = sum(energy_kwh, na.rm = TRUE))
    } else if (input$aggregation_size == "Monthly") {
      solar_data_filtered() |>
        mutate(month = lubridate::floor_date(datetime, unit = "month")) |>
        group_by(month) |>
        summarise(energy_kwh = sum(energy_kwh, na.rm = TRUE))
    } else {
      solar_data_filtered()
    }
  })

  solar_plot <- reactive({
    plot <- ggplot(solar_data_filtered()) +
          labs(y = 'kWh', x = 'Date') +
          theme_minimal()
        if (input$plot_geom == "line") {
          ggplot(solar_data_filtered()) + geom_line(aes(x=datetime, y=energy_kwh)) +
            labs(y='kWh', x='Date') +
            theme_minimal()
        } else if (input$plot_geom == "hist") {
          ggplot(solar_data_aggregated()) + geom_histogram(bins = input$bins, aes(x = energy_kwh)) +
            labs(y='Count', x='Energy (kWh)') +
            theme_minimal()
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

  output$data_table <- renderDT({
    req(solar_data())
    solar_data_filtered()
  })
}

shinyApp(ui, server)