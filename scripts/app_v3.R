#!/usr/bin/env Rscript
#Load necessary packages
library(tidyverse)
library(shiny)
library(png)
library(platetools)
library(viridis)
library(plotly)
library(glue)

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
      
      
      # Input: Selector for choosing plate ----
      selectInput(inputId = "plates",
                  label = "Choose a plate",
                  choices = unique(worm_dat_proc$Metadata_Plate))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: interactive plate plot with worm number as color
      plotlyOutput(outputId = "plate_plot", width = 800, height = 200),
      
      
      # Output: Image linked to plate click
      plotOutput(outputId = "overlay", width = 1000, height = 1000,
                 click = "overlay_click",
                 brush = "overlay_brush")
    )
  )
)

######################################
# Setting up shiny app server format #
######################################

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  # Plot plate that is selected from selector
  output$plate_plot <- renderPlotly({
    ggplotly(ggplot(worm_dat_proc %>% dplyr::filter(Metadata_Plate == input$plates) %>% dplyr::distinct(Metadata_Well, .keep_all = T)) +
      aes(x = factor(Metadata_Column, levels = c(1:12)), y = factor(Metadata_Row, levels = c("G", "F", "E", "D", "C", "B", "A")),
          color = well_count, text = Metadata_Well, key = Metadata_Well) +
      geom_point( size = 2) +
      theme_bw() +
      theme(legend.position = "none") + 
      labs(x = "Column", y = "Row", title = paste("Plate",input$plates)) +
      scale_color_viridis(),
      tooltip = "key")

  })
  
  # Coupled event to plot well image and data point overlay
  output$overlay <- renderPlot({
    
    # Get subset based on selection
    ed <- event_data("plotly_click")
    well <- ed[1,5]
      
      img <- readPNG(glue::glue("~/Desktop/20190723_Ben1/CP_output/RUN_7_FullDose_OUTPUT/20190618_{input$plates}_{well}_w1_overlay.png")) 
      
      h<-dim(img)[1] # image height
      w<-dim(img)[2] # image width
      
      ggplot(worm_dat_proc %>% dplyr::filter(Metadata_Plate == input$plates, Metadata_Well == ed[1,5])) +
             aes(x = AreaShape_Center_X, y = AreaShape_Center_Y, fill = model_select) +
             annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) + # The minus is needed to get the y scale reversed
             scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
             scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
             labs(x = "", y = "", fill = "Model Selection") +
             geom_point(shape = 21, alpha = 0.5) + # The y scale is reversed because in image the vertical positive direction is typically downward
             # Also note the limits where h>0 is the first parameter.
             coord_equal() +
             theme(legend.position = "bottom")
    
    })
  
  # print rows selected or clicked
  output$info <- renderPrint({
    # Get subset based on selection
    ed <- event_data("plotly_click")
    well <- ed[1,5]
    
    rows <- brushedPoints(worm_dat_proc %>% dplyr::filter(Metadata_Plate == input$plates, Metadata_Well == ed[1,5]), input$overlay_brush)
    
  })
  
}

#################
# Call shinyapp #
#################
shinyApp(ui = ui, server = server)