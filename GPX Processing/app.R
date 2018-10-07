####################
##### WholeSheBang
#
#

library(tidyverse)
library(maptools)
library(sp)
library(chron)
library(rgdal)
library(shiny)
library(shinythemes)
library(lubridate)
library(plotly)
library(geosphere)

readGPX <- function(gpxFile){
  
  readOGR(gpxFile, layer="waypoints") %>% # select waypoints from GPX file input
    as.data.frame() %>%
    transmute(
      longitude = coords.x1,
      latitude = coords.x2,
      pointName = name,
      elevation = ele,
      datePoint = as.Date(time), # date only
      timePoint = times(strftime(time,"%H:%M:%S")))
  
}



########################################################################################
########################################################################################
# UI

ui <- navbarPage(
  
             title = "GPX Processing",
             theme=shinytheme("flatly"),

             
             #############################
             # Process points
             tabPanel("Process Points", 
                      
                     fluidRow(
                       column(width=2,
                        fileInput("fileGPX", "Choose GPX File", accept=".GPX") # choose a file
                        ),
                       
                       column(width=2, 
                              uiOutput("select_points") # checkboxes for session points
                              ),
                       
                       column(width=2, 
                              uiOutput("select_trackpoints") # checkboxes for track
                       ),
                       
                       column(width=6, 
                              plotlyOutput("plot_Session")
                              )
                        ),
                     
                     
                     hr(),
                     
                     fluidRow(
                       
                       column(width = 6, 
                              dataTableOutput("PointTable")
                              
                              ),
                       column(width = 6, 
                              h5("Track Length:"),
                              textOutput("text_trackLength"),
                              h5("Direct Distance:"),
                              textOutput("text_dirDis")
                              )
                        )
                      )
 
)


  
########################################################################################
########################################################################################
# Server
  
server <- function(input, output, session) {
   
  

  #########################################################
  # Process Points Tab   
      # select session GPX
      # select points to use
      # create plot
      # track info
      # add a new burrow - link back to other 
  
  
    # Process selected GPX file - all points
  er_GPX_All <- eventReactive(input$fileGPX, {
  
    inFile <- input$fileGPX
    GPXPath <- inFile$datapath
    
    readGPX(GPXPath) # read function
  }) 
  
  

    # render Check Boxes for selecting points
  observeEvent(input$fileGPX, {
    
    pointChoices <- er_GPX_All() %>%  # names of points
      pull(pointName)
    
      # session points
    output$select_points <- renderUI( # render Ui for checkboxes
      checkboxGroupInput("session_points", "Select Session Points",
                         choices=pointChoices, selected=pointChoices) # all choices selected
    )
      
      # track points
    output$select_trackpoints <- renderUI( # render Ui for checkboxes
      checkboxGroupInput("session_trackpoints", "Select Track Points",
                         choices=pointChoices, selected=pointChoices) # all choices selected
    )
    
  })
  
    
    # update session df as checked boxes are de-selected update
  er_GPX <- eventReactive(input$session_points,{

    psel <- input$session_points
    
    er_GPX_All() %>%
      filter(pointName %in% psel)
  })
  
  
    # track df
  er_Track <- eventReactive(input$session_trackpoints,{
    
    tsel <- input$session_trackpoints
    
    er_GPX_All() %>%
      filter(pointName %in% tsel)
    
  })
  
  
  # Track Length
  er_TrackLength <- eventReactive(input$session_trackpoints, {
    
    trackLength(er_Track()) # track length function
    
  }) 
  
  # direct distance dataframe - first and last selected points
  er_DirectDisDF <- eventReactive(input$session_trackpoints, {
    
    d <- er_Track()
    rbind(d[1,], d[nrow(d),]) # first and last row of df

  }) 
  
    # track length and direct distance text
  observeEvent(input$session_trackpoints,{
    
    output$text_trackLength <- renderText(er_TrackLength()) # track length text
   
     output$text_dirDis <- renderText(trackLength(er_DirectDisDF())) # track length text
      
  })
  
    # render table of all GPS points
  output$PointTable <- renderDataTable({er_GPX_All()})
  
    # render plot of points
  observeEvent(input$fileGPX, {
    
    output$plot_Session <- renderPlotly(
      plotSession(er_GPX(), er_Track(), er_DirectDisDF()) # plot points and tracks
    )
    
  })
  
 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

