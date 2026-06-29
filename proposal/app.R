# Load required libraries
library(shiny)
library(leaflet)
library(leaflet.extras)

# 1. Define the Data
incidents_data <- data.frame(
  Location = c(
    "Fatal Tesla Autopilot Crash", 
    "Fatal Electric Skateboard Crash", 
    "Fatal Motorcycle Crash (Wesley Chapel Blvd)", 
    "Major Crash / Highway Closure (I-75 NB)", 
    "Traffic Accident (County Line Rd)", 
    "Traffic Crash / Debris (WB SR-52)", 
    "Overturned Truck (Suncoast Pkwy)", 
    "Traffic Crash (SR-52 & Canyon Blvd)", 
    "Patrol Car Crash (SR-54)", 
    "Rollover Crash with Injuries (US-19 & SR-52)", 
    "Road Closure / Construction (US 41 & SR 54)", 
    "Roadblock (SR-54 & Cattle Ranch Wy)", 
    "Roadblock (SR-56 & Wiregrass Ranch Blvd)", 
    "Roadblock (US-19 & Holiday Hills Blvd)", 
    "Roadblock (Land O Lakes Blvd & SR-54)"
  ),
  City = c(
    "Wesley Chapel", "Wesley Chapel", "Wesley Chapel", "Wesley Chapel", 
    "Wesley Chapel", "Land O Lakes", "Odessa", "Land O Lakes", 
    "Lutz", "Port Richey", "Lutz", "New Port Richey", 
    "Wesley Chapel", "Port Richey", "Land O Lakes"
  ),
  Lat = c(
    28.2376, 28.2376, 28.2257, 28.2435, 28.1677, 
    28.3298, 28.1882, 28.3300, 28.1878, 28.3283, 
    28.1856, 28.1870, 28.1824, 28.2323, 28.1856
  ),
  Lng = c(
    -82.3364, -82.3364, -82.3787, -82.3484, -82.3168, 
    -82.4633, -82.5385, -82.4760, -82.5372, -82.6983, 
    -82.4608, -82.6074, -82.3045, -82.7231, -82.4608
  )
)

# 2. Define the User Interface (UI)
ui <- fluidPage(
  
  titlePanel("Pasco County Traffic Heatmap & Clusters"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Toggle the layers on the map to view the heat density or the interactive clusters."),
      tags$hr(),
      # Sliders for heatmap visuals
      sliderInput("radius", "Heatmap Radius:", min = 5, max = 50, value = 25),
      sliderInput("blur", "Heatmap Blur:", min = 5, max = 50, value = 20)
    ),
    
    mainPanel(
      leafletOutput("trafficMap", height = "600px")
    )
  )
)

# 3. Define the Server Logic
server <- function(input, output, session) {
  
  output$trafficMap <- renderLeaflet({
    
    leaflet(incidents_data) %>%
      # Use Positron for a stark white/light gray background
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -82.45, lat = 28.25, zoom = 11) %>%
      
      # Layer 1: The true Heatmap (Green to Red gradient)
      addHeatmap(
        lng = ~Lng, 
        lat = ~Lat, 
        radius = input$radius,
        blur = input$blur,
        max = 0.05,
        intensity = 1,
        gradient = c("green", "yellow", "red"), # Creates the green-to-red transition
        group = "Heatmap"
      ) %>%
      
      # Layer 2: Interactive Marker Clusters ("n stuff")
      addCircleMarkers(
        lng = ~Lng, 
        lat = ~Lat,
        popup = ~paste("<b>", Location, "</b><br>City:", City),
        clusterOptions = markerClusterOptions(),
        group = "Clusters",
        color = "black", 
        fillColor = "red",
        fillOpacity = 0.8
      ) %>%
      
      # Add a control box so you can toggle them on/off
      addLayersControl(
        overlayGroups = c("Heatmap", "Clusters"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# 4. Run the Application 
shinyApp(ui = ui, server = server)