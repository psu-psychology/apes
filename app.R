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
  
  fluidRow(
    column(
      2,
      numericInput("sample_size_a", "Sample size (n_A)", value = 75, min = 1),
      numericInput(
        "mean_a",
        paste0("Mean (", greeks("mu"), "_A)"),
        value = 0,
        step = 0.1
      ),
      numericInput(
        "sd_a",
        paste0("Std Dev (", greeks("sigma"), "_A)"),
        value = 1,
        min = 0.1,
        step = 0.1
      ),
      hr(), # horizontal rule
      numericInput(
        "alpha",
        label = greeks("alpha"),
        value = 0.05,
        step = .005
      ),
      checkboxInput("paired_samples", "Paired Samples", value = FALSE)
    ),
    column(2,
           numericInput("sample_size_b", "Sample size (n_B)", value = 75, min = 1),
           numericInput(
             "effect_size",
             label = "Effect size (d)",
             value = 0.5,
             step = 0.1
           ),
           numericInput(
             "sd_b",
             paste0("Std Dev (", greeks("sigma"), "_B)"),
             value = 1,
             min = 0.1,
             step = 0.1)
    ),
    column(
      8,
      tabsetPanel(
        type = "tabs",
        tabPanel("Histogram", plotOutput("histPlot")),
        tabPanel("Violin-Box", plotOutput("histBoxPlot")),
        tabPanel("Power curve", plotOutput("powerPlot"))
      )
    )
  ),
  fluidRow(
    column(4,
           actionButton("regenerateData", "Regenerate",
                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
    column(3, skip = 1,
           verbatimTextOutput("ttest")),
    column(3, verbatimTextOutput("power"))
  )
)

#---------------------------------------------

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
            mean = mean_b(), # reactive calculated as sum of mean_a + effect_size
            sd = input$sd_b)
    })
  
  output$histPlot <- renderPlot({
    myhist(sample_a(),
           sample_b(),
           binwidth = .5,
           xlim = c(-5, 5))
  }, res = 96)
  
  output$histBoxPlot <- renderPlot({
    my_box_violin(sample_a(),
           sample_b(),
           binwidth = .5,
           xlim = c(-5, 5))
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(sample_a(),
           sample_b(),
           alpha(),
           var.equal = (input$sd_a == input$sd_b),
           paired = paired_test())
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
  
  paired_test <- reactive({
    input$paired_samples
  })

  output$power <- renderText({
    t_test_power(n1 = input$sample_size_a, 
                 n2 = input$sample_size_b, 
                 d = d(),
                 sig.level = alpha())
  })
  
  
  
  
  output$powerPlot <- renderPlot({
    t_test_power_plot(
      n1 = input$sample_size_a,
      n2 = input$sample_size_b,
      d = d(),
      sig.level = alpha()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
