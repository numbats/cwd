library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

batters <- read.csv("data/wpl_batters.csv")

ui <- page_sidebar(
  title = "WPL batting explorer",
  sidebar = sidebar(
    selectInput(
      "season",
      "Choose a season",
      choices = sort(unique(batters$season))
    ),
    sliderInput(
      "minimum_runs",
      "Minimum runs",
      min = 0,
      max = max(batters$runs),
      value = 150,
      step = 25
    )
  ),
  layout_columns(
    value_box(
      title = "Batters shown",
      value = textOutput("number_shown")
    ),
    value_box(
      title = "Leading run-scorer",
      value = textOutput("leading_batter")
    )
  ),
  plotOutput("batters_plot"),
  tableOutput("batters_table")
)

server <- function(input, output, session) {
  output$number_shown <- renderText({
    batters |>
      filter(
        season == input$season,
        runs >= input$minimum_runs
      ) |>
      nrow()
  })

  output$leading_batter <- renderText({
    filtered <- batters |>
      filter(
        season == input$season,
        runs >= input$minimum_runs
      )

    req(nrow(filtered) > 0)
    filtered$striker[which.max(filtered$runs)]
  })

  output$batters_plot <- renderPlot({
    batters |>
      filter(
        season == input$season,
        runs >= input$minimum_runs
      ) |>
      ggplot(aes(
        x = dot_percent,
        y = boundary_percent,
        size = balls_faced
      )) +
      geom_point(colour = "#006DAE", alpha = 0.7) +
      labs(
        x = "Dot-ball percentage",
        y = "Boundary percentage",
        size = "Balls faced"
      ) +
      theme_minimal(base_size = 14)
  })

  output$batters_table <- renderTable({
    batters |>
      filter(
        season == input$season,
        runs > input$minimum_runs
      ) |>
      arrange(desc(runs)) |>
      select(striker, batting_team, runs, balls_faced) |>
      head(5)
  })
}

shinyApp(ui, server)
