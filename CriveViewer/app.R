library(shiny)
library("shiny")
library("ukpolice")
library("tidyverse")
library("leaflet")

forces <- ukc_forces()


ui <- fluidPage(

    # Application title
    titlePanel("Where are you interested in?"),
    
    sidebarPanel(
      selectInput("force", "Choose which nation force you are interested in?", forces$id),
      uiOutput("area"),
      textInput("date", "Enter the desired year and month in the format YYYY-MM", value = "2023-09"),
    ),
    
    mainPanel(
       leafletOutput("map")
     )
    )


            
?renderUI
            
            

# Define server logic 
server <- function(input, output) {

  
  output$area <- renderUI({
    selectInput("nbd", "Choose which area you are interested in?", ukc_neighbourhoods(input$force)$id)
  })
  
  bdy <- reactive({
    bdy <- ukc_neighbourhood_boundary(input$force, input$nbd)
    bdy |>
      mutate(latitude = as.numeric(latitude),
             longitude = as.numeric(longitude))
  })
  
  crimes <- reactive({
    bdy2 <- bdy() |>
      select(lat = latitude,
             lng = longitude)
    
    ukc_crime_poly(bdy2[round(seq(1, nrow(bdy2), length.out = 100)), ], input$date)
  })

  
  
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(lng = bdy()$longitude, lat = bdy()$latitude) |>
      addCircles(lng = as.numeric(crimes()$longitude), lat = as.numeric(crimes()$latitude), label = crimes()$category, color = "red")
  })
  
  
    
}



# Run the application 
shinyApp(ui = ui, server = server)
