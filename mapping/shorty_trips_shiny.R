library(lubridate)
library(dplyr)
library(openxlsx)
library(reshape)
library(ggthemes)
library(scales)
library(readxl)

#Mapping Libraries
library(tigris)
library(leaflet)
library(sp)
library(maptools)
library(sf)

#Shiny Libraries
library(shiny)
library(shinydashboard)


#Shorty Data ----
##Reading CSV
shorty <- read_excel("data/short_haul_taxi_trips_data.xlsx")

##Getting Year and Month in separate columns
shorty$year_month <- ym(shorty$year_month)
shorty$year <- strftime(shorty$year_month, "%Y")
shorty$month <- strftime(shorty$year_month, "%m")

#Grouping and summarizing data
shorty_grouped <- shorty %>% group_by(year, DOLocationID) %>% summarize(mean_trips = round(mean(trip_count), digit = 0),
                                                                        mean_fare = round(mean(average_fare), digit = 2),
                                                                        mean_length = round(mean(average_length_minutes), digit = 2))

#Taxi Zones Shapefile ----
##Reading shapefile
taxi_zones <- read_sf('data/taxi_zones.shp')
names(taxi_zones)[names(taxi_zones) == 'LocationID'] <- 'DOLocationID'

#Joining airport trips with taxi zone boundaries ----
all_airports_join <- left_join(taxi_zones, shorty, by = 'DOLocationID')

#Shiny UI----
ui2 <- dashboardPage( title = "tlcshortytrips",
  dashboardHeader(title = "Map of Shorty Trips"),
  dashboardSidebar(
    tags$style(HTML("
      .main-sidebar{
        width: 300px;
       
      }
      
      label{color: black;}
      h3{
      padding-left: 15px; padding-right: 15px; color: dimgrey; 
      
      }
      
      h4 { 
      padding-left: 15px; padding-right: 15px; color: dimgrey; 
      }
      
      h5 { 
      padding-left: 15px; padding-right: 15px; color: dimgrey;font-style: italic;
      }
      h6 { 
      padding-left: 15px; padding-right: 15px; color: dimgrey;font-style: italic; margin-top: 30px;
      }
      
    ")),
    h4("1) Select which airport to use as the trip origin."),
    h4("2) Select the time period of trips you would like to see."),
    h5("Note: The color scale resets with each airport and year pairing."),
    
    selectInput('airport', 'Airport:', choices = c('JFK Airport', 'LaGuardia Airport'), selected = 1),
    
    selectInput('year', 'Time Period:', choices = c("August 2019 - January 2020", "August 2021 - January 2022"), selected = 1), 
    
  
    h6("About: The map shows, for each taxi zone, the total number of trips, average fare, and average length for all qualifying 'shorty' trips.", style = "margin-left:10px")
  ),
  dashboardBody(
    
    tags$head(tags$style(HTML('  .skin-blue    .main-sidebar{        
                              width: 300px;
      background-color: #fdf5e6;   }      
                              
                              .main-header > .navbar {        
                                            margin-left: 300px;      }      
                              
                              .main-header .logo {         
                                            width: 300px;      }      
                              
                              .content-wrapper, .main-footer, .right-side { 
                                                                  margin-left: 300px;      }    '))),
    leafletOutput("mymap", height = 710)
   )
  )

  
#Shiny Server ----
server <- function(input, output, session) {
  
  input_airport <- reactive({

    if (input$airport == 'JFK Airport' & input$year == "August 2019 - January 2020") {year1 <- '2019'
                                                                                    year2 <- '2020'
    airport1 <- all_airports_join %>% filter(PULocationID == '132', (year == year1 | year == year2)) %>% group_by(Dropoff_location) %>% summarize(  total_trips = round(sum(trip_count), digit = 0), mean_fare = round(mean(average_fare), digit = 2), mean_length = round(mean(average_length_minutes), digit = 2))} else {
      if (input$airport == 'JFK Airport' & input$year == "August 2021 - January 2022") {year1 <- '2021'
                                                                                      year2 <- '2022'
      airport1 <- all_airports_join %>% filter(PULocationID == '132', (year == year1 | year == year2)) %>% group_by(Dropoff_location) %>% summarize(  total_trips = round(sum(trip_count), digit = 0), mean_fare = round(mean(average_fare), digit = 2), mean_length = round(mean(average_length_minutes), digit = 2))} else {
        
        if (input$airport == 'LaGuardia Airport' & input$year == "August 2019 - January 2020") {year1 <- '2019'
                                                                                              year2 <- '2020'
        airport1 <- all_airports_join %>% filter(PULocationID == '138', (year == year1 | year == year2)) %>% group_by(Dropoff_location) %>% summarize(  total_trips = round(sum(trip_count), digit = 0), mean_fare = round(mean(average_fare), digit = 2), mean_length = round(mean(average_length_minutes), digit = 2))} else {
        
            if (input$airport == 'LaGuardia Airport' & input$year == "August 2021 - January 2022") {year1 <- '2021'
                                                                                                  year2 <- '2022'
        airport1 <- all_airports_join %>% filter(PULocationID == '138', (year == year1 | year == year2)) %>% group_by(Dropoff_location) %>% summarize(  total_trips = round(sum(trip_count), digit = 0), mean_fare = round(mean(average_fare), digit = 2), mean_length = round(mean(average_length_minutes), digit = 2))} else {
          
          }
        }
      }
    }   

    })
  
  
    output$mymap <- renderLeaflet({
      
      if(input$year == "August 2019 - January 2020" & input$airport == 'JFK Airport'){bins <- seq(0, 38000, by = 7600)
      densities <- colorBin("YlOrRd", domain = unique(input_airport()$total_trips), bins = bins)} else {
        if(input$year == "August 2021 - January 2022" & input$airport == 'JFK Airport'){bins <- seq(0, 10000, by = 2000)
        densities <- colorBin("YlOrRd", domain = unique(input_airport()$total_trips), bins = bins)} else{
          if (input$year == "August 2019 - January 2020" & input$airport == 'LaGuardia Airport'){bins <- seq(0, 27000, by = 5400)
          densities <- colorBin("YlOrRd", domain = unique(input_airport()$total_trips), bins = bins)} else{
            if (input$year == "August 2021 - January 2022" & input$airport == 'LaGuardia Airport'){bins <- seq(0, 9000, by = 1800)
            densities <- colorBin("YlOrRd", domain = unique(input_airport()$total_trips), bins = bins)} 
            }
          }
        }
      
      leaflet() %>% addProviderTiles("OpenStreetMap") %>% addPolygons(data = input_airport(), 
                                                                      fillColor = ~densities(input_airport()$total_trips),
                                                                      color = 'black', 
                                                                      weight = 2, 
                                                                      fillOpacity = 0.9,
                                                                      group = '2019 Trips', 
                                                                      #options = pathOptions(clickable = FALSE)
                                                                      
                                                                      popup = paste0("<b>", "DO Location: ", "</b>", input_airport()$Dropoff_location, "<br>",
                                                                                     # "Borough: ", input_airport()$borough, "<br>",
                                                                                     "<b>", "Tot. # of Trips: ", "</b>", input_airport()$total_trips, "<br>",
                                                                                     "<b>", "Avg. Fare: ", "</b>", input_airport()$mean_fare, "<br>",
                                                                                     "<b>", "Avg. Length (mins): ", "</b>", input_airport()$mean_length),
                                                                      highlightOptions = highlightOptions(weight = 4,
                                                                                                          color = "white",
                                                                                                          bringToFront = TRUE)) %>% 
        addLegend(pal = densities, 
                  values = input_airport()$total_trips, 
                  opacity = 0.8, 
                  title = "Total Number of Trips",
                  position = "bottomright")
      
      
    })
  
}


#Run Shiny ----
shinyApp(ui2, server)


# 
# install.packages('rsconnect')
# 
# rsconnect::setAccountInfo(name='phillipwong', token='7BE470C5F76A2C26356A7B5E02E23FC5', secret='CIXJE54xjx72nxr0qYCyJXLixyX66olPmDdjgM46')
# 
# library(rsconnect)
# 
# rsconnect::deployApp('C:\\Users\\WongPh\\Documents\\shorty_tripping_map')

