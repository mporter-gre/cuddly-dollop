#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(EBImage)
library(romero.gateway)
#library(rJava)
source('quickConnect.R')
source('populateDataTree.R')
source('imageHandling.R')


#projTbl <- populateProjects()
#print(projTbl$name)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("OMERO Connection"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    tabsetPanel(
      tabPanel('loginTab', '',
               sidebarPanel(
                 textInput('username', 'Username:'),
                 passwordInput('password', 'Password:'),
                 textInput('host', 'Host URL', value = 'wss://omero.hutton.ac.uk'),
                 numericInput('port', 'Port', value = 443),
                 actionButton('loginBtn', 'Log In'),
                 actionButton('test', 'Test server')
               )
      ),
      tabPanel('browseTab', '',
               sidebarPanel(
                 selectInput("projects",
                             "Projects",
                             choices = 'No projects'),
                 
                 selectInput('datasets',
                             'Datasets',
                             choices = 'No datasets'),
                 
                 selectInput("images",
                             "Images",
                             choices = 'No images'),
                 
                 actionButton('viewImage', 'View image')
               )
      )
    ),
    
    # Main panel UI
    mainPanel(
      fluidRow(
        column(width = 1,
               noUiSliderInput(
                 inputId = "zSlider",
                 orientation = "vertical",
                 min = 1, max = 20,
                 value = 10,
                 width = '50px',
                 height = '500px',
                 format = wNumbFormat(decimals = 0, prefix = "Z=")
               )
        ),
        column(width = 9,
               displayOutput('imgPane'),
               sliderInput("minmaxSlider", "", min = 0, max = 4095, value = c(0, 4095))
        ),
      )
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rVals <- reactiveValues(omConn = NULL, plane = matrix(rpois(100,10),ncol=10))

  observeEvent(input$loginBtn, {
    rVals$omConn <- quickConnect(username = input$username, password = input$password,
                             host = input$host, port = input$port)
    rVals$userId <- getUserId(rVals$omConn)
    projTbl <- populateProjects(rVals$omConn, rVals$userId)
    projList = paste(projTbl$name, projTbl$id, sep = ' | ')
    updateSelectInput(session, 'projects', choices = projList)
    })
  
  observeEvent(input$projects, {
    if(!is.null(rVals$omConn)){
      selectedProjId = sub('.* | ', '', input$projects)
      dsTbl <- populateDatasets(rVals$omConn, selectedProjId)
      dsList = paste(dsTbl$name, dsTbl$id, sep = ' | ')
      updateSelectInput(session, 'datasets', choices = dsList)
    }
  })
  
  observeEvent(input$datasets, {
    if(!is.null(rVals$omConn)){
      selectedDsId = sub('.* | ', '', input$datasets)
      imageTbl <- populateImages(rVals$omConn, selectedDsId)
      imageList <- paste(imageTbl$name, imageTbl$id, sep = ' | ')
      updateSelectInput(session, 'images', choices = imageList)
    }
  })
  
  observeEvent(input$viewImage, {
    selectedImageId = as.integer(sub('.* | ', '', input$images))
    rVals$plane <- getPlane(rVals$omConn, selectedImageId, 1, 1, 1)
    minmax <- range(rVals$plane)
    rVals$rendered <- normalize(rVals$plane, separate=TRUE, ft=c(0,1), inputRange = minmax)
    
    updateSliderInput(session, 'minmaxSlider', value = c(minmax[[1]], minmax[[2]]))
    #updateNoUiSliderInput(session, 'zSlider', enterValueHere)
    
    output$imgPane <- renderDisplay({
      display(rVals$rendered)
    })
  })
  
  observeEvent(input$minmaxSlider, {
    minmax <- input$minmaxSlider
    if (minmax[[1]] != minmax[[2]]){
      rVals$rendered <- EBImage::normalize(rVals$plane, separate=TRUE, ft=c(0,1),
                                           input$minmaxSlider)
      
    }
  })
  

  # output$imgPane <- renderDisplay({
  #   rVals$rendered <- normalize(rVals$plane, separate=TRUE, ft=c(0,1), 
  #                               inputRange = input$minmaxSlider$value)
  #   display(rVals$rendered)
  #   #output$imgPane <- renderDisplay({
  #     
  #   #})
  # })
  
  
  session$onSessionEnded(function() {
    try({
      servConn <- isolate(rVals$omConn)
      disconnect(servConn)
    }, silent = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
