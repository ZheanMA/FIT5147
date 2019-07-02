# ui.R
library(shinythemes)
library(shiny)
library(ggplot2)
library(rgdal)
library(rgeos)
library(plyr)
library(maptools)
library(sp)
library(plotly)
library(networkD3)
library(chorddiag)
library(circlize)
#shiny ui
ui = tagList(
  #set up shiny tab pages and navbar pages
  shinythemes::themeSelector(),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "shinythemes",
    tabPanel('ggplot',
    
    titlePanel("property influence factors: relationship of distance and price in different region area"),
    
    # radio button in selecting different areas and select ggplot view option
    fluidRow(
      column(3, 
             wellPanel(
               selectInput(inputId = "ggplot_type",
                           label = "view type",
                           choices = c('scatterplot','area'),
                           selected = 'scatterplot'),
               radioButtons("Regionname", label="Select region",
                            choices=c('Northern Metropolitan'= 'Northern Metropolitan' , 'Southern Metropolitan'= 'Southern Metropolitan', 'South-Eastern Metropolitan'= 'South-Eastern Metropolitan', 'Western Metropolitan'= 'Western Metropolitan', 
                                      'Eastern Metropolitan'= 'Eastern Metropolitan'), 
                            selected='Eastern Metropolitan'))
             
      ),
      # point and area graph of ggplot 
     mainPanel(12,   
               div(
                 style = "position:relative",
                 plotOutput("trend", 
                            hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                 uiOutput("hover_info")
               )          
             
      )
    )),
    #sankey chart
    tabPanel('sankey',
    titlePanel("Sankey Chart"),
    
    # Row for influence option
    fluidRow(
      column(width = 4, wellPanel(
        radioButtons("sankey_type", "influence option",
                     c("price", "distance")
        )
      )),
      titlePanel("How people choose property when considering Distance/Price.") ,
      sankeyNetworkOutput(outputId ="graph")
             )),
    #chord graph
    tabPanel('chord',
    titlePanel("Chord Chart"),
    
    # Row for influence factor
    fluidRow(
      column(width = 2, wellPanel(
        radioButtons("chord_type", "influence factor",
                     c("price", "distance")
        )
      )),
      titlePanel("Price/Distance factors when buying house in different regions."),
      plotOutput(outputId ="distPlot", height = 700,hover = "plot_hover"),
      
      verbatimTextOutput("info")
      
)),
# a linear regression to predict housing price
    tabPanel('housing property linear regression',
             titlePanel("Based on the analysis before, we can make a linear regression about 
                        the distance to CBD to affect housing price。"), 
             titlePanel(" This is model calculation analysis diagram, which means in data and numerical operation is correct："),
             plotOutput("pic"),
             titlePanel(" This is analysis result："),
             tableOutput("coeff"),
             titlePanel(" linear regression :"),   
             verbatimTextOutput("text"),
             pageWithSidebar(
               headerPanel("Prediction test"),
               sidebarPanel(
                 numericInput("n", "Distance to CBD:", min = 0, max = 40, value = 20),
                 br(),
                 actionButton("goButton", "Go!")
                
               ),
               #forcast results
               mainPanel(
                 verbatimTextOutput("nText")
               )
    )
)))
  