library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  selectInput("var1", "Variable 1", choices = colnames(mtcars), selected = "mpg"),
  selectInput("var2", "Variable 2", choices = colnames(mtcars), selected = "cyl"),
  plotlyOutput("scatter_plot"),
  plotOutput("residual_plot"),
  plotOutput("qq_plot")
)

server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "var2", choices = setdiff(colnames(mtcars), input$var1))
  })

  fit <- reactive({
    form <- as.formula(paste0(input$var2, "~", input$var1))
    broom::augment(lm(form, data = mtcars))
  })

  output$scatter_plot <- renderPlotly({
    g <- ggplot(mtcars, aes(get(input$var1), get(input$var2))) +
        geom_point() + labs(x = input$var1, y = input$var2) +
      geom_smooth(method = "lm", se = FALSE) +
      ggtitle("Scatter plot")
    ggplotly(g)
  })

  output$residual_plot <- renderPlot({
    ggplot(fit(), aes(get(input$var1), .resid)) +
      geom_point() +
      labs(x = input$var1, y = "Residuals") +
      geom_hline(yintercept = 0) +
      ggtitle("Residual plot")
  })

  output$qq_plot <- renderPlot({
    ggplot(fit(), aes(sample = .resid)) +
      geom_qq() + geom_qq_line(color = "red") +
      ggtitle("Q-Q plot of the residuals")
  })
}

shinyApp(ui, server)
