#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("R/util.R")

ui <- fluidPage(
  # Application title
  titlePanel("Alpha, Power, Effect Sizes, Sample Size"),
  
  # Row 0
  fluidRow(column(
    2,
    actionButton("regenerateData", "Regenerate",
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    offset = 1
  ),),
  
  # Row 1
  fluidRow(
    column(1,
           # "A",
           numericInput(
             "sample_size_a", "n_A", value = 75, min = 1
           )),
    column(1,
           # "Criterion & Effect Size",
           numericInput(
             "alpha",
             label = greeks("alpha"),
             value = 0.05,
             step = .001
           )),
    column(1,
           # "B",
           numericInput(
             "sample_size_b", "n_B", value = 75, min = 1
           ))
  ),
  
  # Row 2
  fluidRow(
    column(1,
           numericInput(
             "mean_a",
             paste0(greeks("mu"), "_A"),
             value = 0,
             step = 0.1
           )),
    column(1,
           numericInput(
             "effect_size",
             label = "d",
             value = 0.5,
             step = 0.1
           )),
    column(
      2,
      paste0(greeks("mu"), "_B = ", greeks("mu"), "_A + d", "\n\n"),
      textOutput("out_text")
    )
  ),
  
  # Row 3
  fluidRow(column(
    1,
    numericInput(
      "sd_a",
      paste0(greeks("sigma"), "_A"),
      value = 1,
      min = 0.1,
      step = 0.1
    )
  ),
  column(
    1,
    numericInput(
      "sd_b",
      paste0(greeks("sigma"),
             "_B"),
      value = 1,
      min = 0.1,
      step = 0.1
    )
  )),
  
  # Row 4
  fluidRow(
    column(2, "t test", verbatimTextOutput("ttest")),
    column(2,
           "Power",
           verbatimTextOutput("power")),
    column(8, label = "Histogram", plotOutput("histPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$out_text <- renderText({
    mean_b()
  })
  output$mu_b_label <- renderText({
    paste0(greeks("mu"), "_B")
  })
  
  # (re)generate data
  sample_a <-
    reactive({
      input$regenerateData
      rnorm(
        n = input$sample_size_a,
        mean = input$mean_a,
        sd = input$sd_a
      )
    })
  
  sample_b <-
    reactive({
      input$regenerateData
      rnorm(n = input$sample_size_b,
            mean = mean_b(),
            sd = input$sd_b)
    })
  
  output$histPlot <- renderPlot({
    myhist(sample_a(),
           sample_b(),
           binwidth = .5,
           xlim = c(-5, 5))
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(sample_a(),
           sample_b(),
           alpha(),
           var.equal = (input$sd_a == input$sd_b))
  })
  
  mean_b <- reactive({
    input$mean_a + input$effect_size
  })
  
  n_a <- reactive(length(sample_a))
  n_b <- reactive(length(sample_b))
  d <- reactive(input$effect_size)
  mu_a <- reactive(mean(sample_a()))
  mu_b <- reactive(mean(sample_b()))
  alpha <- reactive(input$alpha)
  
  output$power <- renderText({
    t_test_power(length(sample_a()), length(sample_b()), d())
  })
  
  
  # output$powerPlot <- renderPlot({
  #   t_test_power_plot(length(sample_a()), d())
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
