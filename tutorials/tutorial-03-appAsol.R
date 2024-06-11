library(shiny)

result <- tagList()

ui <- fluidPage(
  actionButton("flip", "Flip coin 100 times"),
  htmlOutput("estimate")
)

server <- function(input, output, session) {
  out <- reactive({
    if (input$flip) {
      sample(c("H", "T"),
        size = 100,
        replace = TRUE, prob = c(0.8, 0.2)
      )
    }
  })

  output$estimate <- renderUI({
    if (input$flip) {
      new <- div(
        strong("Estimate of the probability of getting a head:"),
        round(mean(out() == "H"), 3),
        div(paste0(out(), collapse = " "))
      )
      result <<- tagAppendChild(result, new)
      result
    }
  })
}

shinyApp(ui, server)
