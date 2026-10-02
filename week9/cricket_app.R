library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

batters <- read.csv("data/wpl_batters.csv")

ui <- page_sidebar(
  title = "How do WPL batters score their runs?",
  theme = bs_theme(preset = "shiny", primary = "#006DAE"),

  sidebar = sidebar(
    selectInput(
      "season",
      "Choose a season",
      choices = sort(unique(batters$season)),
      selected = max(batters$season)
    ),
    helpText("The chart includes the top 20 run-scorers with at least 50 balls faced.")
  ),

  card(
    card_header(textOutput("plot_title")),
    p("Batters towards the upper-left combine more boundaries with fewer dot balls."),
    plotOutput("batters_plot", height = "520px"),
    card_footer(
      markdown("Data courtesy of [Cricsheet](https://cricsheet.org/) via the `cricketdata` package.")
    )
  )
)

server <- function(input, output, session) {
  selected_batters <- reactive({
    req(input$season)

    batters |>
      filter(season == input$season)
  })

  output$plot_title <- renderText({
    paste("Batting patterns in", input$season)
  })

  output$batters_plot <- renderPlot({
    ggplot(
      selected_batters(),
      aes(x = dot_percent, y = boundary_percent, size = balls_faced)
    ) +
      geom_point(colour = "#006DAE", alpha = 0.65) +
      labs(
        x = "Dot-ball percentage",
        y = "Boundary percentage",
        size = "Balls faced"
      ) +
      scale_x_continuous(labels = scales::label_percent(scale = 1)) +
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      theme_minimal(base_size = 15) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "right"
      )
  })
}

shinyApp(ui, server)
