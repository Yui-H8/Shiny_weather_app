# Load required libraries
require(leaflet)

# Create a RShiny UI
shinyUI(
  fluidPage(padding = 5,
            titlePanel("Current weather and forecast temperatures"),
            # Create a side-bar layout
            sidebarLayout(
              # Main panel to show cities on a leaflet map
              mainPanel(
                leafletOutput("city_weather_map", height = 1000)
              ),
              # Sidebar to show detailed plots for a city
              sidebarPanel(
                # City selection dropdown
                selectInput(
                  inputId = "city_dropdown",
                  label = "Cities",
                  choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris", 
                              "Tokyo", "Moscow", "Canberra", "Vancouver"),
                  selected = "All"
                ),
                plotOutput("temp_line", height = "300px", width = "100%"),
                br(),
                plotOutput("wind_area_humidity", height = "300px", width = "100%")
              )
            )
  )
)
