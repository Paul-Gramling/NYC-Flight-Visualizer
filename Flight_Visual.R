library(nycflights13)
library(shiny)
library(leaflet)
library(dplyr)

data("flights")
data("airports")
data("airlines") 


####cleaning/joining data####

combined_data <- flights |>
  left_join(airlines, by = "carrier") |>
  left_join(airports, by = c("dest" = "faa")) |>
  left_join(airports, by = c("origin" = "faa")) |>
  rename(lat_origin = lat.y, lon_origin = lon.y,
         lat_dest = lat.x, lon_dest = lon.x,
         airport_origin = name, airport_dest = name.y,
         airline_name = name.x) |>
  mutate(total_delay = dep_delay + arr_delay,
         Status = ifelse(total_delay >= 0 & total_delay <= 14, "On Time",
                         ifelse(total_delay < 0, "Early", ifelse(total_delay >= 15, "Late", NA))))

# Convert month numbers to words
combined_data$month <- factor(combined_data$month, 
                              levels = 1:12,
                              labels = c("January", "February", "March", "April", "May", "June", 
                                         "July", "August", "September", "October", "November", "December"))

# Group and filter by airport
destination_counts <- combined_data |>
  group_by(origin, airport_dest) |>
  summarize(count = n()) |>
  ungroup() |>
  filter(!is.na(airport_dest))

top_destinations <- destination_counts |>
  arrange(desc(count)) |>
  group_by(origin) |>
  slice_head(n = 20) |>
  ungroup()

filtered_data <- combined_data |>
  semi_join(top_destinations, by = c("origin", "airport_dest"))



####creation of dashboard####

ui <- fluidPage(
  titlePanel("NYC Flight Visualizer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:",
                  choices = unique(filtered_data$month)),
      selectInput("day", "Select Day:",
                  choices = unique(filtered_data$day)),
      selectInput("airline", "Select Airline:",
                  choices = unique(filtered_data$airline_name)),
      selectInput("airport", "Select Origin Airport:",
                  choices = unique(filtered_data$origin))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)


server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -95.7129, lat = 37.0902, zoom = 4)
  })
  
  observe({
    req(input$month, input$day, input$airline_name, input$origin)
    
    filtered_data <- filtered_data |>
      filter(month == input$month &
               day == input$day &
               airline_name == input$airline_name &
               origin %in% c("JFK", "LGA", "EWR"))
    
    leafletProxy("map", data = filtered_data) |>
      clearShapes() |>
      addPolylines(
        lng = ~lon_origin,
        lat = ~lat_origin,
        lng2 = ~lon_dest,
        lat2 = ~lat_dest,
        color = ~ifelse(Status == "Early", "green",
                        ifelse(Status == "On Time", "blue", "red")),
        weight = 2, opacity = 1
      ) |>
      addMarkers(
        lng = ~lon_origin,
        lat = ~lat_origin,
        popup = ~paste("Origin: ", airport_origin)
      ) |>
      addMarkers(
        lng = ~lon_dest,
        lat = ~lat_dest,
        popup = ~paste("Destination: ", airport_dest)
      )
  })
  
}


shinyApp(ui, server)




