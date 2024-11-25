library(shiny)

ui <- fluidPage(
  titlePanel("Carbon Emissions and Threatened Species Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("prosperity", "Select Prosperity Category:",
                  choices = NULL,  # Start with no choices; they'll be updated dynamically
                  selected = "All"),
      sliderInput("species_range", "Filter by Number of Threatened Species:",
                  min = 0, max = 1000, value = c(0, 1000)),
      br(),
      actionButton("refresh", "Refresh Plots")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplots",
                 plotOutput("scatterplot1"),
                 plotOutput("scatterplot2")),
        tabPanel("Histograms",
                 plotOutput("histogram")),
        tabPanel("Model Summary",
                 verbatimTextOutput("model_summary"))
      )
    )
  )
)
