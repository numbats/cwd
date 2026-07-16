library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(plotly)

ipldata <- cricketdata::fetch_cricsheet("bbb", "female", "wpl")

summarise_best_batters <- function(.data) {
  .data |>
    group_by(season, batting_team, striker) |>
    summarise(
      runs = sum(runs_off_bat),
      balls_faced = n() - sum(!is.na(wides)),
      strikerate = runs/balls_faced * 100,
      dot_percent = (sum(runs_off_bat == 0) * 100) / balls_faced,
      boundary_percent = (sum(runs_off_bat %in% c(4,6)) * 100) / balls_faced
    )
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      /* tighten the sidebar a bit */
      .well { padding: 12px; }
      /* keep labels compact */
      .control-label { margin-bottom: 6px; }
    "))
  ),

  titlePanel("Women's Premier League Match Explorer"),

  # Make the sidebar narrower (3/12) and the main panel wider (9/12)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput(
        "top_batters",
        "Number of top batters",
        min = 1, max = 50, value = 20
      ),
      selectizeInput(
        "season",
        "Season",
        choices = sort(unique(ipldata$season)),
        selected = sort(unique(ipldata$season)),
        multiple = TRUE,
        options = list(placeholder = "Choose season(s)")
      )
    ),
    mainPanel(
      width = 9,
      # Let the plot breathe vertically
      plotlyOutput("distPlot", height = "75vh"),
      markdown("Data courtesy of [cricsheet.org](cricsheet.org) via the `cricketdata` package"),
      downloadButton("download_plot")

    )
  )
)

server <- function(input, output, session) {

  label_percent <- scales::label_percent(accuracy = 0.1)


  plot <- reactive({
    req(input$season, input$top_batters)

    ipldata |>
      summarise_best_batters() |>
      ungroup() |>
      filter(boundary_percent > 0, season %in% input$season) |>
      arrange(desc(runs)) |>
      slice(1:input$top_batters) |>
      mutate(
        text = paste0("Dot Balls: ", label_percent(dot_percent/100),
                      "<br>Strike Rate: ", round(strikerate, digits = 1),
                      "<br>Boundaries: ", label_percent(boundary_percent/100),
                      "<br>Runs: ", runs,
                      "<br>Balls Faced: ", balls_faced,
                      ifelse(length(input$season) > 1, paste0("<br>Year: ", season), ""))
      ) |>
      ggplot(
        aes(text = text)
      ) +
      geom_point(
        aes(y = boundary_percent, x = dot_percent, size = 2*balls_faced),
        colour = "red", alpha = 0.3) +
      geom_text(
        aes(y = boundary_percent, x = dot_percent, label = striker),
        vjust = -0.5, hjust = 0.5, color = "#013369",
        position = position_dodge(0.9), size = 5
      ) +
      labs(x = "Dot Percent", y = "Boundary Percent") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(face = "bold")
      )
  }
  )
  output$distPlot <- renderPlotly({

    ggplotly(plot(), tooltip = "text")
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("wpl-top",input$top_batters,"-",input$season, ".png")
    },

    content = function(file) {
      ggsave(plot = plot(), filename = file, bg = "white")
    }
  )
}

shinyApp(ui = ui, server = server)