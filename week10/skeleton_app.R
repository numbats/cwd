library(shiny)

ui <- fluidPage(
  headerPanel("Solar dashboard"),
  sidebarPanel(
    sliderInput("date_range", "Subset dates", min = as.Date('2024-07-15'), max = as.Date('2025-10-05'),
                value = c(as.Date('2024-07-15'), as.Date('2025-10-05'))),
  ),
  mainPanel(
    plotOutput("solar_plot")
  )
)

server <- function(input, output) {
  solar_data <- read.csv(here::here("data", "smartmeter.csv")) |>
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

  output$solar_plot <- renderPlot({
    solar_data <- solar_data |>
      filter(datetime >= input$date_range[1] & datetime <= input$date_range[2])
    ggplot(solar_data, aes(x = datetime, y = energy_kwh)) +
      geom_line() +
      labs(y = 'kWh', x = 'Date') +
      theme_minimal()
  })
}

shinyApp(ui, server)