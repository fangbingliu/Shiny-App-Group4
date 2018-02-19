library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(maps)
library(DT)

##########################Load Data###############################
Recommendation <- read.csv("../data/project_data/Recommendation.csv", header = T, stringsAsFactors = F)
Rank1 <- Recommendation[12:21, ]
Rank2 <- Recommendation[1:10, ]

shinyServer(function(input, output, session){
  
#################Explore page(Fangbing Liu)#######################
  
  # Need to change the "value" with what you want to show as a result
  # Need to change render"Print" to other functions
  # Enter Location 
  output$value <- renderPrint({ 
    input$Location })
  
  # Choose Activities
  output$value <- renderPrint({ 
    input$Activities })

###############Recommendation Page(Fangbing Liu)##################

  #The top 10 tourist attractions rank
  output$Rank1 <- renderDataTable({
  
    datatable(Rank1[, c("Rank", "Name")], rownames = FALSE)%>% formatStyle(
      'Name', 
      target = 'row', color = 'black', backgroundColor = 'lightpurple')
    })
  
  #Map
  output$maprec1 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 12) %>%
      addMarkers(lng = as.numeric(Rank1$longitude),
                 lat = as.numeric(Rank1$latitude),
                 popup = paste("Rank:", Rank1$Rank, "<br>",
                               "Name:", Rank1$Name, "<br>",
                               "Address:", Rank1$Address, "<br>",
                               "Description:", Rank1$Description)
      )
  })
  
  #Top 10 Restaurant rank
  output$Rank2 <- renderDataTable({
    
    datatable(Rank2[, c("Rank", "Name")], rownames = FALSE)%>% formatStyle(
      'Name', 
      target = 'row', color = 'black', backgroundColor = 'lightpurple')
    })
  
  #Map
  output$maprec2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 12) %>%
      addMarkers(lng = as.numeric(Rank2$longitude),
                 lat = as.numeric(Rank2$latitude),
                 popup = paste("Rank:", Rank2$Rank, "<br>",
                               "Name:", Rank2$Name, "<br>",
                               "Address:", Rank2$Address, "<br>",
                               "Description:", Rank2$Description)
      )
  })
})