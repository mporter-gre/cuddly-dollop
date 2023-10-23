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
      tabPanel('Login', '',
               sidebarPanel(
                 textInput('username', 'Username:'),
                 passwordInput('password', 'Password:'),
                 textInput('host', 'Host URL', value = 'wss://omero.hutton.ac.uk'),
                 numericInput('port', 'Port', value = 443),
                 actionButton('loginBtn', 'Log In')
               )
      ),
      tabPanel('Browse', '',
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
                 direction = 'rtl',
                 update_on = 'end',
                 format = wNumbFormat(decimals = 0, prefix = "Z=")
               )
        ),
        column(width = 9,
               displayOutput('imgPane'),
               #radioGroupButtons('channelRadios'),
               tags$div(id = 'channelButtons'),
               sliderInput("minmaxSlider", "", min = 0, max = 4095, value = c(0, 4095)),
               plotOutput('histPlot')
        ),
      )
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #A collection of useful values
  rVals <- reactiveValues(omConn = NULL, plane = matrix(rpois(100,10),ncol=10), 
                          sizeZ = 1, selectedImageId = 1, channelButtons = c(),
                          selectedC = 1)
  
  #Log in and populate Projects
  observeEvent(input$loginBtn, {
    rVals$omConn <- quickConnect(username = input$username, password = input$password,
                                 host = input$host, port = input$port)
    rVals$userId <- getUserId(rVals$omConn)
    projTbl <- populateProjects(rVals$omConn, rVals$userId)
    projList = paste(projTbl$name, projTbl$id, sep = ' | ')
    updateSelectInput(session, 'projects', choices = projList)
  })
  
  #Populate Datasets
  observeEvent(input$projects, {
    if(!is.null(rVals$omConn)){
      selectedProjId = sub('.* | ', '', input$projects)
      dsTbl <- populateDatasets(rVals$omConn, selectedProjId)
      dsList = paste(dsTbl$name, dsTbl$id, sep = ' | ')
      updateSelectInput(session, 'datasets', choices = dsList)
    }
  })
  
  #Populate Images
  observeEvent(input$datasets, {
    if(!is.null(rVals$omConn)){
      selectedDsId = sub('.* | ', '', input$datasets)
      imageTbl <- populateImages(rVals$omConn, selectedDsId)
      imageList <- paste(imageTbl$name, imageTbl$id, sep = ' | ')
      updateSelectInput(session, 'images', choices = imageList)
    }
  })
  
  #View selected image
  observeEvent(input$viewImage, {
    if (!is.null(rVals$omConn)){
      #Define the image object and dimensions
      rVals$selectedImageId = as.integer(sub('.* | ', '', input$images))
      rVals$imgObj <- loadObject(rVals$omConn, 'ImageData', rVals$selectedImageId)
      rVals$imgDims <- getImageDimensions(rVals$imgObj)
      sizeZ <- rVals$imgDims[[3]]
      
      #Get and render the plane
      rVals$plane <- getPlane(rVals$omConn, rVals$selectedImageId, sizeZ/2, 1, 1)
      rVals$minmax <- range(rVals$plane)
      rVals$rendered <- normalize(rVals$plane, separate=TRUE, ft=c(0,1), inputRange = rVals$minmax)
      
      #Update the rendering minMaxSlider
      updateSliderInput(session, 'minmaxSlider', value = c(rVals$minmax[[1]], rVals$minmax[[2]]))
      
      #Update the zSlider
      updateNoUiSliderInput(session, 'zSlider', range = c(1, sizeZ), value = round(sizeZ/2))
      
      #Update channel buttons
      #Clear existing buttons, if present
      if (length(rVals$channelButtons) > 0){
        sizeC <- length(rVals$channelButtons)
        for (channel in 1:sizeC){
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', rVals$channelButtons[[channel]])
          )
        }
        rVals$channelButtons <- c()
      }
      
      #Create new channel buttons
      channelNamesList <- getChannelNames(rVals$imgObj)
      for (channel in 1:rVals$imgDims[[4]]){
        buttonId <-paste0('channel', channel)
        insertUI(
          selector = '#channelButtons',
          ui = actionButton(buttonId, channelNamesList[[channel]])
        )
        rVals$channelButtons <- c(buttonId, rVals$channelButtons)
      }

      #Populate the histogram
      #histData <- hist(rVals$plane, plot = FALSE)
      output$histPlot <- renderPlot(
        hist(rVals$plane, plot = TRUE),
        # print(rVals$histData)
        #plot(rVals$histData)#, type='b')
      )
      
      #Display the image
      output$imgPane <- renderDisplay({
        display(rVals$rendered)
      })
    }
  })
  
  #Update render settings change
  observeEvent(input$minmaxSlider, {
    rVals$minmax <- input$minmaxSlider
    if (rVals$minmax[[1]] != rVals$minmax[[2]]){
      rVals$rendered <- EBImage::normalize(rVals$plane, separate=TRUE, ft=c(0,1), input$minmaxSlider)
    }
  })
  
  #Update on z change
  observeEvent(input$zSlider, {
    if (!is.null(rVals$omConn)){
      rVals$plane <- getPlane(rVals$omConn, rVals$selectedImageId, input$zSlider, 1, rVals$selectedC)
      rVals$rendered <- EBImage::normalize(rVals$plane, separate=TRUE, ft=c(0,1), rVals$minmax)
    }
  })
  
  #Update on channel change. Not ideal reactive logic here - investigate
  observeEvent(input$channel1, {
    rVals$selectedC = 1
    rVals$plane <- getPlane(rVals$omConn, rVals$selectedImageId, input$zSlider, 1, rVals$selectedC)
    rVals$rendered <- EBImage::normalize(rVals$plane, separate=TRUE, ft=c(0,1), rVals$minmax)
  })
  observeEvent(input$channel2, {
    rVals$selectedC = 2
    rVals$plane <- getPlane(rVals$omConn, rVals$selectedImageId, input$zSlider, 1, rVals$selectedC)
    rVals$rendered <- EBImage::normalize(rVals$plane, separate=TRUE, ft=c(0,1), rVals$minmax)
  })
  observeEvent(input$channel3, {
    rVals$selectedC = 3
    rVals$plane <- getPlane(rVals$omConn, rVals$selectedImageId, input$zSlider, 1, rVals$selectedC)
    rVals$rendered <- EBImage::normalize(rVals$plane, separate=TRUE, ft=c(0,1), rVals$minmax)
  })
  observeEvent(input$channel4, {
    rVals$selectedC = 4
    rVals$plane <- getPlane(rVals$omConn, rVals$selectedImageId, input$zSlider, 1, rVals$selectedC)
    rVals$rendered <- EBImage::normalize(rVals$plane, separate=TRUE, ft=c(0,1), rVals$minmax)
  })
  
  
  
  #Close OMERO session when browser is closed
  session$onSessionEnded(function() {
    try({
      servConn <- isolate(rVals$omConn)
      disconnect(servConn)
    }, silent = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
