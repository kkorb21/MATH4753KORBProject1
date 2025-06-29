#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(MATH4753KORBProject1)  # Load your package

# Helper function to produce both plots using base graphics
render_optimal_plots <- function(N, p, gamma, n_range, step) {
  par(mfrow = c(2, 1))  # Two plots, one above the other

  # Generate the plots using the package function
  optimal_n(N = N, p = p, gamma = gamma, n_range = n_range, step = step)
}

ui <- fluidPage(
  titlePanel("Optimal Ticket Sales: Binomial and Normal Approximation"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("N", "Venue Capacity (N):", value = 200, min = 50, max = 300, step = 10),
      sliderInput("p", "Show-up Probability (p):", min = 0.5, max = 1, value = 0.95, step = 0.01),
      sliderInput("gamma", "Risk Tolerance (γ):", min = 0.01, max = 0.5, value = 0.2, step = 0.01),
      sliderInput("nmin", "Min n in Range:", min = 50, max = 300, value = 200, step = 1),
      sliderInput("nmax", "Max n in Range:", min = 50, max = 300, value = 220, step = 1),
      numericInput("step", "Step Size:", value = 1, min = 1, max = 10)
    ),

    mainPanel(
      plotOutput("plots", height = "700px"),
      verbatimTextOutput("results")
    )
  )
)

server <- function(input, output) {

  output$results <- renderPrint({
    optimal_n(N = input$N,
              p = input$p,
              gamma = input$gamma,
              n_range = c(input$nmin, input$nmax),
              step = input$step)
  })

  output$plots <- renderPlot({
    old_par <- par(no.readonly = TRUE)  # Save current par settings
    on.exit(par(old_par))               # Restore after plotting

    render_optimal_plots(
      N = input$N,
      p = input$p,
      gamma = input$gamma,
      n_range = c(input$nmin, input$nmax),
      step = input$step
    )
  })
}

shinyApp(ui = ui, server = server)
