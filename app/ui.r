if (!require("shinythemes")) install.packages('shinythemes')
if (!require("DT")) install.packages('DT')
if (!require("dtplyr")) install.packages('dtplyr')
if(!require("lubridate")) install.packages('lubridate')
library(shiny)
library(leaflet)
library(shinythemes)
library(markdown)
library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)

##################Home Page(Fangbing Liu)#####################
shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("journal"),
    navbarPage("Explore NYC!",
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
                              selectInput("variable", "Location:",
                                          list("Time Square" = "Time Square", 
                                               "Columbia University" = "Columbia University", 
                                               "Empire State Building" = "Empire State Building",
                                               "Wall Street"="Wall Street",
                                               "Central Park"="Central Park",
                                               "New York University"="New York University",
                                               "John F. Kennedy International Airport"="John F. Kennedy International Airport",
                                               "LaGuardia Airport"="LaGuardia Airport"),
                                          selected = "Columbia University"),
                            
                            # Choose Activities
                            checkboxGroupInput("type", label = h4("Activities:"), 
                                               choices = list("Restaurant" = "restaurant", "Shopping" = "shop", "Museum/Gallery" = "museum", 
                                                              "Library" = "library", "Sightseeing" = "history", "WIFI" = "wifi","Park" = "park","Theater" = "theater"),
                                               selected = NULL),
                            
                            # Choose Radius
                            sliderInput("DIST", label = h4("Distance From Your Starting Point(km):"),
                                        min = 1, max = 20, value = 3, step = 0.5)),
                          # Map
                          mainPanel(
                            fluidRow(
                              column(6,DT::dataTableOutput("table")),
                              
                             # column(6, verbatimTextOutput('sitestogo')),
                              
                              hr(),
                              column(6, verbatimTextOutput('orderofsites')),
                              column(6,leafletOutput("map",width = "120%",height = 450))
                            ))
                        )
               ),
               ############Recommendation Page(Fangbing Liu)#################                    
               
               tabPanel("Recommendation",
                        tabsetPanel(
                          
                          tabPanel("Top 10 Tourist Attractions",
                                   mainPanel(
                                     fluidRow(
                                       
                                       column(6,
                                              dataTableOutput("Rank1")),
                                       
                                       column(6,
                                              leafletOutput("maprec1", width = "200%", height = 600)),
                                       
                                       helpText(a("For more information, click here", href = "http://www.planetware.com/tourist-attractions-/new-york-city-us-ny-nyc.htm"))
                                       
                                     )
                                   )),
                          
                          tabPanel("Top 10 Hottest Restaurants in 2018",
                                   mainPanel(
                                     fluidRow(
                                       
                                       column(6,
                                              dataTableOutput("Rank2")),
                                       
                                       column(6,
                                              leafletOutput("maprec2", width = "200%", height = 600)),
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
)