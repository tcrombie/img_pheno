#!/usr/bin/env Rscript
#Load necessary packages
library(tidyverse)
library(shiny)
library(png)
library(platetools)
library(viridis)
library(plotly)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

##################################
# Data pre-processing            #
##################################
load('data/proc_pheno.Rda')

##########################################################
# Potting worm centroids for each model on image         #
##########################################################
# subset data for testing this is temporary
df <- worm_dat_proc %>%
  dplyr::filter(strain == "N2",
                Metadata_Plate == 1)

wells <- df %>%
  dplyr::distinct(Metadata_Plate, Metadata_Well, .keep_all = TRUE)

##################################
# Functions and colors for plots #
##################################
# Include alpha function for showing overplotting
## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# Define colors
myColours = c("#00BFC4", "#F8766D")
myColoursAlpha <- add.alpha(myColours, alpha=0.5)

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
    sidebarPanel(width =2,
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "bin num",
                  min = 1,
                  max = 50,
                  value = 30),
      
      # Input: Selector for choosing plate ----
      selectInput(inputId = "plates",
                  label = "Choose a plate",
                  choices = unique(worm_dat_proc$Metadata_Plate)),
      
      # Input: Selector for choosing well ----
      selectInput(inputId = "well",
                  label = "Choose a plate",
                  choices = unique(worm_dat_proc$Metadata_Well))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: interactive plate plot with worm number as color
      plotOutput(outputId = "plate_plot"),
      
      # Output: Image ----
      plotOutput(outputId = "overlay", width = 1000, height = 1000),
      
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
  
  # Plot histogram of worm number per well
  output$distPlot <- renderPlot({
    
    x    <- df %>% dplyr::distinct(Metadata_Plate, Meatadata_Well, .keep_all =T) %>% .$well_count
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Worm number per well",
         main = "Histogram of worm counts")
    
  })
  
  # Plot image of well and overlay centroid
  output$overlay <- renderPlot({
    # Plot image and overlay using png package and base R. Another option would be to use ggplot https://stackoverflow.com/questions/4993189/overlay-data-onto-background-image
    
    img_control <- as.raster(png::readPNG("~/Desktop/20190723_Ben1/CP_output/RUN_7_FullDose_OUTPUT/20190618_1_A01_w1_overlay.png")) #read PNG as raster in R
    
    plot(img_control, xlim = c(0, 2048), ylim = c(2048,0)) #plot raster with correct dimensions
    symbols(x = df$AreaShape_Center_X,
            y = df$AreaShape_Center_Y,
            circles = rep(5,nrow(df)), inches = FALSE, add = TRUE, bg=myColoursAlpha[2]) # draw points from selected model
    
  })
  
  # Plot plate that is selected from selector
  output$plate_plot <- renderPlot({
    
    plate_df <- df %>%
      dplyr::filter(Metadata_Plate == input$plates)
    
    raw_map(data = plate_df$well_count,
            well = df$Metadata_Well,
            plate = 96) + 
      labs(title =paste("Plate",input$plates),  fill = "well count") +
      theme_dark() +
      scale_fill_viridis()
    
  })
  
}



#################
# Call shinyapp #
#################
shinyApp(ui = ui, server = server)