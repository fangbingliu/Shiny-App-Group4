packages.used=c("shiny", "shinythemes", "shinydashboard", "dplyr", 
                "leaflet","maps", "DT", "dtplyr", "lubridate", "TSP", "maptools", "stringr",
                "bitops", "ggmap", "ggplot2")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,
                   repos='http://cran.us.r-project.org')
}

library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(maps)
library(DT)
library(dtplyr)
library(lubridate)
library(TSP)
library(maptools)
library(stringr)
library(bitops)
library(ggmap)
library(ggplot2)

source( "./Route acc Type.R" )

##########################Load Data###############################
Recommendation <- read.csv("../data/project_data/Recommendation.csv", header = T, stringsAsFactors = F)
Rank1 <- Recommendation[12:21, ]
Rank2 <- Recommendation[1:10, ]
dataframe<-read.csv("../data/alldata.csv")

####################Calculate Distance(Peifeng Hong)#######################

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

  # Choose Radius(Peifeng Hong)
  
  ##starting point ( to be changed )
  
  startlat <- start$latitude
  startlon <- start$longtitude
  
  #distance to start point
  disttosites <- rep(NA,nrow(sitesdataset))
  for (i in 1:nrow(sitesdataset))
     disttosites[i] <- distcalculate(startlat,startlon,sitesdataset$latitude[i],sitesdataset$longtitude[i])
  
  
   siteswithinrange_rec <- reactive({
     siteswithinrange <- filter(data.frame(sitesdataset,
                                           dist = disttosites),disttosites < input$DIST, type %in% input$type)
     siteswithinrange})
  
  ##dataset with name 
  output$table <- DT::renderDataTable(siteswithinrange_rec()[,c("name","type","dist")],server = T)
  
  output$sitestogo <- renderPrint({
    cat('\nSelected Sites\n')
     print(siteswithinrange_rec()[input$table_rows_selected,c('name','dist')])
  })   
  
 
  
 # sitesmap <- routeplan(sitestogo_rec(),30000)
  
  # MAP
  route_df <- reactive({
    route_df = route_df
  })
  
  #rownames(route_df)<- c(1:nn)
  
  my_list <- reactive({
    my_list <- list()
    r <- 1
    for (i in 1:(nn-1)) {
      for (j in ((i+1):nn)) {
        my_route <- viaroute(route_df$latitude[i], route_df$longtitude[i],route_df$latitude[j], route_df$longtitude[j])
        geom <- decode_geom(my_route$routes[[1]]$geometry)
        my_list[[r]] <- geom
        r <- r + 1
      }
    }
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles("Stamen.TonerLite") %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 12) %>%
      addLayersControl(baseGroups = c("OSM", "Stamen.TonerLite")) %>%
      addCircleMarkers(lat = route_df$latitude,
                       lng = route_df$longtitude,
                       color = "red",
                       stroke = FALSE,
                       radius = 5,
                       fillOpacity = 0.8) %>%
      addLayersControl(baseGroups = c("OSM", "Stamen.TonerLite")) %>%
      {
        for (i in 1:length(my_list)) {
          . <- addPolylines(., lat = my_list[[i]]$lat, lng = my_list[[i]]$lng, color = "red", weight = 4)
        }
        return(.)
      }
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
      addCircleMarkers(lng = as.numeric(Rank1$longitude),
                 lat = as.numeric(Rank1$latitude),
                 popup = paste("<b>Rank:</b>", Rank1$Rank, "<br>",
                               "<b>Name:</b>", Rank1$Name, "<br>",
                               "<b>Address:</b>", Rank1$Address, "<br>",
                               "<b>Description:</b>", Rank1$Description)) %>%
      addPopups(lng = as.numeric(Rank1$longitude[input$Rank1_rows_selected]),
                lat = as.numeric(Rank1$latitude[input$Rank1_rows_selected]),
                popup = paste("<b>Rank:</b>", Rank1$Rank[input$Rank1_rows_selected], "<br>",
                              "<b>Name:</b>", Rank1$Name[input$Rank1_rows_selected], "<br>",
                              "<b>Address:</b>", Rank1$Address[input$Rank1_rows_selected], "<br>",
                              "<b>Description:</b>", Rank1$Description[input$Rank1_rows_selected]),
                options = popupOptions(closeButton = FALSE))
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
      addCircleMarkers(lng = as.numeric(Rank2$longitude),
                 lat = as.numeric(Rank2$latitude),
                 popup = paste("<b>Rank:</b>", Rank2$Rank, "<br>",
                               "<b>Name:</b>", Rank2$Name, "<br>",
                               "<b>Address:</b>", Rank2$Address, "<br>",
                               "<b>Description:</b>", Rank2$Description)) %>%
      addPopups(lng = as.numeric(Rank2$longitude[input$Rank2_rows_selected]),
                lat = as.numeric(Rank2$latitude[input$Rank2_rows_selected]),
                popup = paste("<b>Rank:</b>", Rank2$Rank[input$Rank2_rows_selected], "<br>",
                              "<b>Name:</b>", Rank2$Name[input$Rank2_rows_selected], "<br>",
                              "<b>Address:</b>", Rank2$Address[input$Rank2_rows_selected], "<br>",
                              "<b>Description:</b>", Rank2$Description[input$Rank2_rows_selected]))
  })
})