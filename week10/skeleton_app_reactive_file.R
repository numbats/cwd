library(shiny)
library(DT)

ui <- fluidPage(
  headerPanel("Solar dashboard"),
  sidebarPanel(
    fileInput("solar_file", "Upload solar data CSV", accept = c(".csv")),
    sliderInput("date_range", "Subset dates", min = as.Date('2024-07-15'), max = as.Date('2025-10-05'),
                value = c(as.Date('2024-07-15'), as.Date('2025-10-05'))),
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("solar_plot")),
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

  output$solar_plot <- renderPlot({
    ggplot(solar_data_filtered(), aes(x = datetime, y = energy_kwh)) +
      geom_line() +
      labs(y = 'kWh', x = 'Date') +
      theme_minimal()
  })

  output$data_table <- renderDT({
    req(solar_data())
    solar_data_filtered()
  })
}

shinyApp(ui, server)