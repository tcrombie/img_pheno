#!/usr/bin/env Rscript
#Load necessary packages
library(tidyverse)
library(shiny)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# test shiny examples
runExample("01_hello")
# test shiny examples
runExample("02_text")
# test shiny examples
runExample("03_reactivity")
# test shiny examples
runExample("04_mpg")
# test shiny examples
runExample("05_sliders")
# test shiny examples
runExample("06_tabsets")
# test shiny examples
runExample("07_widgets")
# test shiny examples
runExample("08_html")
##################################
# Setting up shiny app UI format #
##################################

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("img-pheno"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Hist number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

######################################
# Setting up shiny app server format #
######################################

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

#################
# Call shinyapp #
#################
shinyApp(ui = ui, server = server)

