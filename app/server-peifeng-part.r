#preparation
if (!require("DT")) install.packages('DT')
if (!require("dtplyr")) install.packages('dtplyr')
if(!require("lubridate")) install.packages('lubridate')
library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)
library(shiny)


##function calculating the distance of start points and sites
Pi <- 3.14159
distcalculate <- function(startlat,startlon,destlat,destlon){
  
  angle <- sin(startlat)*sin(destlat) + cos(startlat)*cos(destlat)*cos(startlon-destlon)
  
  distance <-  6378.137*acos(angle)*Pi/180
  
  return( round(distance,digits = 2))
}

##dataset
sitesdataset <- read.csv("../data/data.csv",stringsAsFactors = F)

##shinyserver
shinyServer(function(input, output,session) {
  
  #  output$select <- renderPrint({ input$select })
  
  #  output$otx <- renderPrint({
  #    input$btn
  #    input$lnk
  #    isolate(input$itx)
  #  })
  
  
  ##starting point ( to be changed )
  
  startlat <- 40.78
  startlon <- (-73.81)
  
  #distance to start point
  disttosites <- rep(NA,nrow(sitesdataset))
  for (i in 1:nrow(sitesdataset))
    disttosites[i] <- distcalculate(startlat,startlon,sitesdataset$latitude[i],sitesdataset$longtitude[i])
  

  siteswithinrange_rec <- reactive({
    siteswithinrange <- filter(data.frame(sitesdataset[,c('name','type')],
                                          Dist = disttosites),disttosites < input$DIST, type %in% input$type)
    siteswithinrange})
  
  ##dataset with name 
  output$table <- DT::renderDataTable(siteswithinrange_rec(),server = T)
  
  output$sitestogo <- renderPrint({
    cat('\nSelected Sites\n')
    print(siteswithinrange_rec()[input$table_rows_selected,c('name','Dist')])
  })                            
})