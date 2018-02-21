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

##################Home Page(Fangbing Liu)#####################
shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("journal"),
    navbarPage("NYC Trip Planner",
             tabPanel("Home",
                      mainPanel(fluidRow(
                        column(9, align="center", offset = 3,
                               tags$iframe(width = "720", height = "480", align = "middle", src = "https://www.youtube.com/embed/A-_uX2cdUdM",
                                           frameborder = "0", allowfullscreen = "allowfullscreen")),
                        column(12, align="center", offset = 6,
                               h4("Click Explore to Start!", align = "center", style = "color:#FF0000")))),
                      tags$head(
                        tags$style(HTML("body{background-image: url(background.jpg);}")))),
             
##################Explore page(Fangbing Liu)###################             
             tabPanel("Explore",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          
                          # Introduction
                          helpText("You can enter your location.(Ex: Columbia University), 
                                   then choose what activities you want to do and how far you want to go."),
                          
                          # Enter Location
                          textInput("input", label = h4("Location:"), value = "Enter location..."),
                          
                          # Choose Activities
                          checkboxGroupInput("type", label = h4("Activities:"), 
                                            choices = list("Restaurant" = 1, "Shopping" = 2, "Museum/Gallery" = 3, 
                                                           "Library" = 4, "Sightseeing" = 5, "WIFI" = 6),
                                            selected = NULL)
                          ),
                          
                          # Choose Radius
                          sliderInput("DIST", label = "Distance From Your Starting Point(km):",
                                      min = 1, max = 20, value = 3, step = 0.5),
                          hr(),
                         
                          # Map
                          mainPanel(
                            
                            fluidRow(
                              column(6, DT::dataTableOutput('table')),
                              column(6, verbatimTextOutput('sitestogo'))
                            ),
                            leafletOutput("route", width = "110%", height = 600))
                            #leafletOutput("map", width = "110%", height = 600)
                        )
                      )),
            
############Recommendation Page(Fangbing Liu)#################                    
                                  
             tabPanel("Recommendation",
                      tabsetPanel(
                        
                        tabPanel("Top 10 Tourist Attractions",
                                 mainPanel(
                                   fluidRow(
                                     
                                     column(6,
                                            dataTableOutput("Rank1")),
                                           
                                     column(6,
                                            leafletOutput("maprec1", width = "200%", height = 580)),
                                     
                                     helpText(a("For more information, click here", href = "http://www.planetware.com/tourist-attractions-/new-york-city-us-ny-nyc.htm"))
                                     
                                     )
                                   )),
                        
                        tabPanel("Top 10 Hottest Restaurants in 2018",
                                 mainPanel(
                                   fluidRow(
                                     
                                     column(6,
                                            dataTableOutput("Rank2")),
                                            
                                     column(6,
                                            leafletOutput("maprec2", width = "200%", height = 580)),
                                     helpText(a("For more information, click here", href = "https://ny.eater.com/maps/best-new-nyc-restaurants-heatmap"))
                                     )
                                 ))
                        )),
             
###################About US Page(Fangbing Liu)#################

             tabPanel("About Us",
                      fluidRow(
                        column(7, img(src='thankyou.gif', align = "right")),
                        column(8, align="center", offset = 2, includeMarkdown("contact.md"))
                        )
                      )
))

       
    