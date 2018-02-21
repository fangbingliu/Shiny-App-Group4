if (!require("DT")) install.packages('DT')
if (!require("dtplyr")) install.packages('dtplyr')
if(!require("lubridate")) install.packages('lubridate')
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(maps)
library(DT)
library(dtplyr)
library(lubridate)

##########################Load Data###############################
Recommendation <- read.csv("../data/project_data/Recommendation.csv", header = T, stringsAsFactors = F)
Rank1 <- Recommendation[12:21, ]
Rank2 <- Recommendation[1:10, ]

####################Calculate Distance(Peifeng Hong#######################

##function calculating the distance of start points and sites
Pi <- 3.14159
distcalculate <- function(startlat,startlon,destlat,destlon){
  
  angle <- sin(startlat)*sin(destlat) + cos(startlat)*cos(destlat)*cos(startlon-destlon)
  
  distance <-  6378.137*acos(angle)*Pi/180
  
  return( round(distance,digits = 2))
}

##dataset
sitesdataset <- read.csv("../data/alldata.csv",stringsAsFactors = F)


##############Shiny Server##############

shinyServer(function(input, output, session){
  
#################Explore page#######################

  # Enter Location 
  output$value <- renderPrint({ 
    input$Location })
  
  # Choose Activities
  output$value <- renderPrint({ 
    input$Activities })
  
  # Choose Radius(Peifeng Hong)
  
  ##starting point ( to be changed )
  
 # startlat <- 40.78
 #  startlon <- (-73.81)
  
  #distance to start point
 # disttosites <- rep(NA,nrow(sitesdataset))
 # for (i in 1:nrow(sitesdataset))
 #    disttosites[i] <- distcalculate(startlat,startlon,sitesdataset$latitude[i],sitesdataset$longtitude[i])
  
  
 #  siteswithinrange_rec <- reactive({
 #    siteswithinrange <- filter(data.frame(sitesdataset[,c('name','type')],
 #                                          Dist = disttosites),disttosites < input$DIST, type %in% input$type)
 #    siteswithinrange})
  
  ##dataset with name 
 # output$table <- DT::renderDataTable(siteswithinrange_rec(),server = T)
  
 # output$sitestogo <- renderPrint({
 #   cat('\nSelected Sites\n')
 #    print(siteswithinrange_rec()[input$table_rows_selected,c('name','Dist')])
 # })   
  
  # Choose Number
 # output$value <- renderPrint({
 #    input$Number
 #  })
  
  # MAP
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 12)
      })

###############Recommendation Page(Fangbing Liu)##################

  #The top 10 tourist attractions rank
  output$Rank1 <- renderDataTable({
  
    datatable(Rank1[, c("Rank", "Name")], rownames = FALSE)%>% formatStyle(
      'Name', 
      target = 'row', color = 'black', backgroundColor = 'lightpurple')
    }, server = TRUE)
  
  #Map
  output$maprec1 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.9712, lat = 40.7580, zoom = 12) %>%
      addMarkers(lng = as.numeric(Rank1$longitude),
                 lat = as.numeric(Rank1$latitude),
                 popup = paste("Rank:", Rank1$Rank, "<br>",
                               "Name:", Rank1$Name, "<br>",
                               "Address:", Rank1$Address, "<br>",
                               "Description:", Rank1$Description)) %>%
      addPopups(lng = as.numeric(Rank1$longitude[input$Rank1_rows_selected]),
                lat = as.numeric(Rank1$latitude[input$Rank1_rows_selected]),
                popup = paste("Rank:", Rank1$Rank[input$Rank1_rows_selected], "<br>",
                              "Name:", Rank1$Name[input$Rank1_rows_selected], "<br>",
                              "Address:", Rank1$Address[input$Rank1_rows_selected], "<br>",
                              "Description:", Rank1$Description[input$Rank1_rows_selected]))
  })
  
  #Top 10 Restaurant rank
  output$Rank2 <- renderDataTable({
    
    datatable(Rank2[, c("Rank", "Name")], rownames = FALSE)%>% formatStyle(
      'Name', 
      target = 'row', color = 'black', backgroundColor = 'lightpurple')
    }, server = TRUE)
  
  #Map
  output$maprec2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.9712, lat = 40.7500, zoom = 13) %>%
      addMarkers(lng = as.numeric(Rank2$longitude),
                 lat = as.numeric(Rank2$latitude),
                 popup = paste("Rank:", Rank2$Rank, "<br>",
                               "Name:", Rank2$Name, "<br>",
                               "Address:", Rank2$Address, "<br>",
                               "Description:", Rank2$Description)) %>%
      addPopups(lng = as.numeric(Rank2$longitude[input$Rank2_rows_selected]),
                lat = as.numeric(Rank2$latitude[input$Rank2_rows_selected]),
                popup = paste("Rank:", Rank2$Rank[input$Rank2_rows_selected], "<br>",
                              "Name:", Rank2$Name[input$Rank2_rows_selected], "<br>",
                              "Address:", Rank2$Address[input$Rank2_rows_selected], "<br>",
                              "Description:", Rank2$Description[input$Rank2_rows_selected]))
  })
})