library(shiny)
library("shiny")
library("ukpolice")
library("tidyverse")
library("leaflet")

forces <- ukc_forces()


ui <- fluidPage(

    # Application title
    titlePanel("UK Monthly Crime Viewer"),
    
    sidebarPanel(
      selectInput("force", "Which Police Force?", forces$id),
      uiOutput("area"),
      textInput("date", "Enter the Desired Month in YYYY-MM format", value = "2023-09"),
    ),
    
    mainPanel(
       leafletOutput("map")
     )
    )


            
?renderUI
            
            

# Define server logic 
server <- function(input, output) {

  
  output$area <- renderUI({
    selectInput("nbd", "Which Area Within the Force?", ukc_neighbourhoods(input$force)$id)
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
