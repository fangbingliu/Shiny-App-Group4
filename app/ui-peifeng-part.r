library(shiny)

fluidPage(
  title = 'DataTables Information',
## distance to staring point
  sidebarPanel(
    sliderInput("DIST", label = "Distance to starting point:",
                min = 1, max = 20, value = 3, step = 0.5),
  hr(),
## type of sites
  checkboxGroupInput("type", label = "types of sites",
                   choices=c("restaurant" = "restaurant","library" = "library","park" = "park","wifi" = "wifi"),
                   selected=1
                   )
  ),
  mainPanel(
  h1('A table using server-side processing'),
  verbatimTextOutput('test'),
  fluidRow(
    column(6, DT::dataTableOutput('table')),
    column(6, verbatimTextOutput('sitestogo'))
  )
)
)