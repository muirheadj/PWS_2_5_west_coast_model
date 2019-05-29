shinyUI(fillPage(

  # Application title
  titlePanel("Population growth dashboard"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
      sidebarPanel(
          numericInput(inputId = "param", label = "Parameter", value = 2,
              min = 1, max = 2, step = 1)
          ),
    # Show a plot of the generated distribution
    mainPanel(
  #    textOutput(outputId = "param_text"),
      h3("Mean non-seed ports population size"),
       plotlyOutput(outputId = "port_pop_Plot", height = "400px"),
      h3("Non-seed ports number of invaded ports"),
      plotlyOutput(outputId = "port_n_Plot", height = "400px")
    )
  )
))
