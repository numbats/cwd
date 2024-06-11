library(shiny)
library(ggplot2)
library(purrr)

calculate_confint <- function(n, mean, sd, level, dist, N = 100) {
  map_dfr(seq(N), ~ {
    # draw samples
    x <- switch(dist,
      Normal = rnorm(n, mean, sd),
      Exponential = rexp(n, 1 / mean)
    )
    alpha <- 1 - level / 100
    mean_est <- mean(x)
    error_bound <- qt(1 - alpha / 2, n - 1) * sd(x) / sqrt(n)
    data.frame(
      sample = .x,
      mean = mean_est,
      lower = mean_est - error_bound,
      upper = mean_est + error_bound
    )
  })
}


ui <- fluidPage(
  selectInput("dist", "What distribution is the population?",
    selected = "Normal",
    choices = c("Normal", "Exponential")
  ),
  numericInput("mean", "What is the population mean?", value = 1, step = 0.1),
  uiOutput("dynamic_sd_input"), # only show if Normal distribution
  numericInput("n", "How many samples?", value = 30, step = 1),
  numericInput("level", "What percentage level of confidence interval?",
    value = 95, step = 1, min = 0, max = 100
  ),
  plotOutput("plot"),
  br(),
  textOutput("number"),
  br(),
  dataTableOutput("table")
)

server <- function(input, output, session) {
  df <- reactive({
    req(input$n, input$mean, input$sd, input$level, input$dist)
    calculate_confint(input$n, input$mean, input$sd, input$level, input$dist)
  })

  output$dynamic_sd_input <- renderUI({
    if (input$dist == "Normal") {
      numericInput("sd", "What is the population standard deviation?", value = 1, step = 1)
    }
  })

  output$plot <- renderPlot({
    ggplot(df(), aes(x = mean, y = sample)) +
      geom_point() +
      geom_errorbarh(aes(xmin = lower, xmax = upper)) +
      geom_vline(xintercept = input$mean, color = "red")
  })

  output$number <- renderText({
    ntrue <- sum(input$mean <= df()$upper & input$mean >= df()$lower)
    paste("There are ", ntrue, "out of 100 confidence intervals that contain the population mean.")
  })

  output$table <- renderDataTable({
    df()
  })
}

shinyApp(ui, server)
