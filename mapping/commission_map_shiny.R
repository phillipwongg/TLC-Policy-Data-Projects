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

library(shiny)
library(shinydashboard)


#Taxi Zone Level Commission Analysis Data ----
##Reading in excel spreadsheet "Commission_Analysis_Pickups2.xlsx"
company_pickups <- lapply(1:3, function(i) read_excel("data/Commission_Analysis_Pickups2.xlsx", sheet = i)) 

##Industry sheet----
industry <- data.frame(company_pickups[[1]]) 
##Renaming Columns
names(industry)[names(industry) == 'Commission.Rate.Tier'] <-'commish_tier'
names(industry)[names(industry) == 'PULocationID'] <- 'LocationID'
names(industry)[names(industry) == 'Total.Subsidized.Trips'] <- 'tot_subsidized_trips'
names(industry)[names(industry) == 'Total.Amount.Subsidized...'] <- 'tier_amount_subsidized'
names(industry)[names(industry) == 'Total.Trips.in.Location'] <- 'tot_trips'
names(industry)[names(industry) == 'Overall..Total.in.Location'] <- 'tot_amount_subsidized'
names(industry)[names(industry) == 'Percentage.of.Trips.Subsidized'] <- 'percent_subsidized'
##Calculating percentage of trips subsidized in each tier
industry$percent_sub <- round(100 * industry$percent_subsidized, digits = 2)
industry$tier_amount_subsidized <- abs(industry$tier_amount_subsidized)

##Uber sheet
uber <- data.frame(company_pickups[[2]])
##Renaming columns
names(uber)[names(uber) == 'Commission.Rate.Tier'] <-'commish_tier'
names(uber)[names(uber) == 'PULocationID'] <- 'LocationID'
names(uber)[names(uber) == 'Total.Subsidized.Trips'] <- 'tot_subsidized_trips'
names(uber)[names(uber) == 'Total.Amount.Subsidized...'] <- 'tier_amount_subsidized'
names(uber)[names(uber) == 'Total.Trips.in.Location'] <- 'tot_trips'
names(uber)[names(uber) == 'Overall..Total.in.Location'] <- 'tot_amount_subsidized'
names(uber)[names(uber) == 'Percentage.of.Trips.Subsidized'] <- 'percent_subsidized'
##Calculating percentage of trips subsidized in each tier
uber$percent_sub <- round(100 * uber$percent_subsidized, digits = 2)
uber$tier_amount_subsidized <- abs(uber$tier_amount_subsidized)

##Lyft sheet
lyft <- data.frame(company_pickups[[3]])
##Renaming columns
names(lyft)[names(lyft) == 'Commission.Rate.Tier'] <-'commish_tier'
names(lyft)[names(lyft) == 'PULocationID'] <- 'LocationID'
names(lyft)[names(lyft) == 'Total.Subsidized.Trips'] <- 'tot_subsidized_trips'
names(lyft)[names(lyft) == 'Total.Amount.Subsidized...'] <- 'tier_amount_subsidized'
names(lyft)[names(lyft) == 'Total.Trips.in.Location'] <- 'tot_trips'
names(lyft)[names(lyft) == 'Overall..Total.in.Location'] <- 'tot_amount_subsidized'
names(lyft)[names(lyft) == 'Percentage.of.Trips.Subsidized'] <- 'percent_subsidized'
##Calculating percentage of trips subsidized in each tier
lyft$percent_sub <- round(100 * lyft$percent_subsidized, digits = 2)
lyft$tier_amount_subsidized <- abs(lyft$tier_amount_subsidized)


#Loading Taxi Zones ----
##Taxi Zone Boundaries
taxi_zones <- read_sf("data/taxi_zones.shp")
##Removing Newark Airport from list of taxi zones
taxi_zones <- taxi_zones[-1,]

#Joining Zones and data ----
##Spatial dataframe for inudstry level data
commission_geo_industry <- left_join(taxi_zones, industry, by = 'LocationID')

##Spatial dataframe for uber data
commission_geo_uber <- left_join(taxi_zones, uber, by = 'LocationID')

##Spatial dataframe for lyft data
commission_geo_lyft <- left_join(taxi_zones, lyft, by = 'LocationID')


#Shiny UI: ----
ui <- dashboardPage( title = "commission_report_map",
                      dashboardHeader(title = "Commission Report Map"),
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
                        h4("1) Select HV Company"),
                        h4("2) Select commission tier"),
                        h5("Note: The color scale resets with each company and tier pairing."),
                        selectInput('company', 'HV Company', choices = c("All HV Trips", "Uber", "Lyft")),
                        #selectInput('borough', 'Borough:', choices = c('All Boroughs', 'Bronx', 'Manhattan', 'Queens', 'Brooklyn', 'Staten Island'), selected = 1),
                        
                        selectInput('commission', 'Commission Tier:', choices = c("0 to -10%", "-10 to -20%", "-20 to -30%", "-30 to -40%", "-40 to -50%", "-50% +"), selected = 1), 
                        
                        
                        
                        h6("About: The map shows the percent of trips to each taxi zone that are subsidized by HV companies at the rate of the selected commission tier. A subsidized trip means the HV company paid the driver more than the rider paid in total, thus a negative commission for the company.", style = "margin-left:10px")),
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
                        leafletOutput("mymap", height="90vh"), 
                        tableOutput("table")))


#Shiny Server----
server <- function(input, output, session) {

  input_company <- reactive({
    
    if (input$company == 'Uber') {
      company <- commission_geo_uber  %>% filter(tot_subsidized_trips > 1)
      
    } else{
      if (input$company == 'Lyft') {
        company <- commission_geo_lyft %>% filter(tot_subsidized_trips > 1)
      } else{
        if (input$company == 'All HV Trips') {
          company <- commission_geo_industry  %>% filter(tot_subsidized_trips > 1)
        }
      }
    }
    
    
  })
  
  
   input_tier <- reactive({
     
     if (input$commission == "0 to -10%") {
       tier <- input_company() %>% filter(commish_tier == '-10% to 0%') 
       # bins <- c(1.6, 2.6, 3.2, 3.8, 4.4, 5.0, 5.6)
       # densities <- colorBin("YlOrRd", domain = tier$percent_sub, bins = bins)
       
     } else {
       if (input$commission == "-10 to -20%") {
         tier <- input_company() %>% filter(commish_tier == '-10% to -20%')
         
       } else {
         if (input$commission == "-20 to -30%") {
           tier <- input_company() %>% filter(commish_tier == '-20% to -30%')
           
         } else {
           if (input$commission == "-30 to -40%") {
             tier <- input_company() %>% filter(commish_tier == '-30% to -40%')
             
           } else {
             if (input$commission == "-40 to -50%") {
                 tier <- input_company() %>% filter(commish_tier == '-40% to -50%')
                 
               } else {
               if (input$commission == "-50% +") {
                 tier <- input_company() %>% filter(commish_tier == '-50% +')
                 
             }
       
             }
         }
       }
       }
     }
   })
   
 
  
   output$mymap <- renderLeaflet({
   
 
     bins <- seq(min(input_tier()$percent_sub), max(input_tier()$percent_sub), by = (max(input_tier()$percent_sub) - min(input_tier()$percent_sub))/ 5)
     densities <- colorBin("YlOrRd", domain = input_tier()$percent_sub, bins = bins)
     
     leaflet() %>% addProviderTiles("OpenStreetMap") %>% addPolygons(data = input_tier(), 
                                                                     fillColor = ~densities(input_tier()$percent_sub),
                                                                     color = 'black', 
                                                                     weight = 2, 
                                                                     fillOpacity = 0.9,
                                                                      popup = paste0("<b>", "Pickup Location: ", "</b>", input_tier()$zone, "<br>",
                                                                                     "<b>", "% Trips Subsidized in Tier: ", "</b>", input_tier()$percent_sub,"%", "<br>",
                                                                                     "<b>", "# of Trips in Tier: ", "</b>",  formatC(input_tier()$tot_subsidized_trips, format = "d", big.mark = ","),  "<br>",
                                                                                     "<b>", "Total # of Trips in Zone: ", "</b>", formatC(input_tier()$tot_trips, format = "d", big.mark = ","), "<br>",
                                                                                     "<b>", "Total $ Subsidized in Tier: ", "</b>", "-$", scales::number_format(big.mark = ",")(input_tier()$tier_amount_subsidized)),
                                                                     highlightOptions = highlightOptions(weight = 4,
                                                                                                         color = "white",
                                                                                                         bringToFront = TRUE)) %>% 
       addLegend(pal = densities,
                 values = input_tier()$percent_sub,
                 opacity = 0.8, 
                 title = "Percent of Trips Subsidized",
                 position = "bottomright",
                 labFormat =  labelFormat(suffix = "%")

                 )
     
     
   })
  
}

#Run Shiny ----
shinyApp(ui, server)
