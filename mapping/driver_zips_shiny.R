library(lubridate)
library(data.table)
library(dplyr)
library(openxlsx)
library(reshape)
library(ggthemes)
library(scales)
library(readxl)
library(stringr)
#library(splitr)
library(tidyr)
library(dplyr)

#Mapping Libraries
library(tigris)
library(leaflet)
library(sp)
library(maptools)
library(sf)
library(shiny)
library(shinydashboard)
#library(rgdal)

#Scraping Websites for Representatives ----

##City Council:
library(base)
library(rvest)

scraping_council <- read_html("https://council.nyc.gov/districts/")
cc_districts <- scraping_council %>% html_elements( ".sort-district") %>% html_text()
cc_reps <- scraping_council %>% html_elements( ".sort-member") %>% html_text()
cc_boroughs <- scraping_council %>% html_elements( ".sort-borough") %>% html_text()

cc_representatives <- data.frame(unlist(cc_districts), unlist(cc_reps), unlist(cc_boroughs))
names(cc_representatives) <- c("CounDist", "Council Member", "Borough")
###Converting to numeric for future left joins on CounDist column
cc_representatives$CounDist <- as.numeric(cc_representatives$CounDist)

##State Assembly:
scraping_assembly <- read_html("https://nyassembly.gov/mem/")
sa_names_nums <- scraping_assembly %>% html_elements( ".mem-name") %>% html_text2() 
step2 <- gsub("\r", "", sa_names_nums) 
sa_reps <- sub("\\ District.*", "", step2)
sa_districts <- sub(".* District ", "", step2) %>% trimws()

sa_representatives <- data.frame(unlist(sa_districts), unlist(sa_reps))
names(sa_representatives) <- c("AssemDist", "Assembly Member")
sa_representatives$AssemDist <- as.numeric(sa_representatives$AssemDist)

##State Senate:
scraping_senate <- read_html("https://www.nysenate.gov/senators-committees")
senators <- scraping_senate %>% html_elements( ".nys-senator--name") %>% html_text2() 
sen_districts <- scraping_senate %>% html_elements( ".nys-senator--district") %>% html_text2()

step2 <- sub(".*) ", "", sen_districts) 
step3 <- sub("\\ District.*", "", step2)
step4 <- sub("(\\(st|nd|rd|th)\\b", "", step3)

ss_representatives <- data.frame(unlist(step4), unlist(senators))
names(ss_representatives) <- c("StSenDist", "State Senator")
ss_representatives[43,1] = 21
ss_representatives[24,1] = 31
ss_representatives$StSenDist <- as.numeric(ss_representatives$StSenDist)

##Congress:
scraping_congress <- read_html("https://www.ny.gov/new-york-state-congressional-delegation")
congress_people <- scraping_congress %>% html_nodes('a') %>% html_text2()
congressional <- congress_people[8:34] 
scraping_congress_dists <- read_html("https://ballotpedia.org/United_States_congressional_delegations_from_New_York")
congress_districts <- scraping_congress_dists %>% html_nodes('a') %>% html_text2() 
list_dist <- congress_districts[49:102] 
only_districts <- list_dist[seq(1, length(list_dist), 2L)]
only_districts <- sub(".*District ", "", only_districts)

congress_delegation <- data.frame(unlist(only_districts), unlist(congressional))
names(congress_delegation) <- c("CongDist", "Congress Person")
congress_delegation$CongDist <- as.numeric(congress_delegation$CongDist)

#Zip Code Data----

##Loading driver zip codes
driver_zips <- read_excel("data/driversinallzips_cleaned.xlsx", col_types = c('text', 'text'))

#Adding 0 to zips that are < 5 digits (excel does not read leading zero)
driver_zips$zip <- ifelse(((nchar(driver_zips$zip)) < 5), (paste0("0", driver_zips$zip)), driver_zips$zip)
driver_zips$Total <- as.numeric(driver_zips$Total)
#Renaming zip code column in preparation for future left_join with Zip code shapefile
names(driver_zips)[1] <- 'ZIPCODE'


##NYC Zip Code Boundaries:
#unzip("ZIP_CODE_040114.zip")
nyc_zips <- read_sf('data/ZIP_CODE_040114.shp')
#Left joining zip code boundaries with total count of drivers in each zip
ny_zips_drivers <- left_join(nyc_zips, driver_zips, by = 'ZIPCODE')
#Center of zip codes
zips_center <- st_centroid(ny_zips_drivers)
#Transforming boundaries
nyc_zips_transform <- st_transform(ny_zips_drivers, "+init=epsg:4326")


#State Senate Data: ----
##Shapefile
ss_shp <- read_sf('data/nyss_22a1/nyss.shp')
ss_transform <- st_transform(ss_shp, "+init=epsg:4326")

##Intersection of Congressional Districts with Zip Codes
ss_council_intersection <- st_intersection(x = ny_zips_drivers, y = st_buffer(ss_shp, -100))
ss_council_intersection_df <- as.data.frame(ss_council_intersection) %>% select(c("ZIPCODE", "StSenDist"))
ss_council_intersection_df <- ss_council_intersection_df %>% group_by(StSenDist) %>% mutate(id = row_number()) %>% pivot_wider(names_from = 'id', values_from = 'ZIPCODE')
ss_council_intersection_df$Zips_represented <- apply(ss_council_intersection_df[ , 2:46] , 1 , paste , collapse = ", " )
ss_council_intersection_df$Zips_represented <- str_replace_all(ss_council_intersection_df$Zips_represented, "NA", "")

#Need to run 9 times to remove all trailing ","
for (i in 1:46){
  ss_council_intersection_df$Zips_represented <- gsub(", $", "", ss_council_intersection_df$Zips_represented)
}
ss_and_zips_df2 <- ss_council_intersection_df %>% select("StSenDist", "Zips_represented")
ss_zips_reps2 <- left_join(ss_representatives, ss_and_zips_df2, by = "StSenDist")

ss_zips_center <- st_intersection(x = zips_center, y = ss_shp)
ss_zips_center_reps <- left_join(ss_zips_center, ss_representatives, by = 'StSenDist')

#Congressional District Data: ----
##Shapefile
#unzip("nycg_22a.zip")
cd_shp <- read_sf('data/nycg_22a/nycg.shp')
cd_transform <- st_transform(cd_shp, "+init=epsg:4326")

##Intersection of Congressional Districts with Zip Codes
cd_council_intersection <- st_intersection(x = ny_zips_drivers, y = st_buffer(cd_shp, -100))
cd_council_intersection_df <- as.data.frame(cd_council_intersection) %>% select(c("ZIPCODE", "CongDist"))
cd_council_intersection_df <- cd_council_intersection_df %>% group_by(CongDist) %>% mutate(id = row_number()) %>% pivot_wider(names_from = 'id', values_from = 'ZIPCODE')
cd_council_intersection_df$Zips_represented <- apply(cd_council_intersection_df[ , 2:73] , 1 , paste , collapse = ", " )
cd_council_intersection_df$Zips_represented <- str_replace_all(cd_council_intersection_df$Zips_represented, "NA", "")

#Need to run 9 times to remove all trailing ","
for (i in 1:75){
  cd_council_intersection_df$Zips_represented <- gsub(", $", "", cd_council_intersection_df$Zips_represented)
}
cd_and_zips_df2 <- cd_council_intersection_df %>% select("CongDist", "Zips_represented")
cd_zips_reps2 <- left_join(congress_delegation, cd_and_zips_df2, by = "CongDist")

##Finding main representative for zip code
cd_zips_center <- st_intersection(x = zips_center, y = cd_shp)
cd_zips_center_reps <- left_join(cd_zips_center, congress_delegation, by = 'CongDist')

#Assembly District Data ----
##Shapefile
#unzip("nyad_22a.zip")
 ad_shp <- read_sf('data/nyad_22a/nyad.shp')
 ad_transform <- st_transform(ad_shp, "+init=epsg:4326")

##Intersection of Assembly Districts and Zip Codes
ad_council_intersection <- st_intersection(x = ny_zips_drivers, y = st_buffer(ad_shp, -100))
ad_council_intersection_df <- as.data.frame(ad_council_intersection) %>% select(c("ZIPCODE", "AssemDist"))
ad_council_intersection_df <- ad_council_intersection_df %>% group_by(AssemDist) %>% mutate(id = row_number()) %>% pivot_wider(names_from = 'id', values_from = 'ZIPCODE')
ad_council_intersection_df$Zips_represented <- apply(ad_council_intersection_df[ , 2:32] , 1 , paste , collapse = ", " )
ad_council_intersection_df$Zips_represented <- str_replace_all(ad_council_intersection_df$Zips_represented, "NA", "")

#Need to run 9 times to remove all trailing ","
for (i in 1:32){
  ad_council_intersection_df$Zips_represented <- gsub(", $", "", ad_council_intersection_df$Zips_represented)
}
ad_and_zips_df2 <- ad_council_intersection_df %>% select("AssemDist", "Zips_represented")
sa_representatives$AssemDist <- as.numeric(sa_representatives$AssemDist)
ad_zips_reps2 <- left_join(sa_representatives, ad_and_zips_df2, by = "AssemDist")

##Finding main representative for zip codes
ad_zips_center <- st_intersection(x = zips_center, y = ad_shp)
ad_zips_center_reps <- left_join(ad_zips_center, sa_representatives, by = 'AssemDist')


#City Council Data: ----
##Shapefile
#unzip("nycc_22a.zip")
cc_shp <- read_sf('data/nycc_22a/nycc.shp')
cc_transform <- st_transform(cc_shp, "+init=epsg:4326")
 
cc_zips_center <- st_intersection(x = zips_center, y = cc_shp)
 
cc_zips_center_reps <- left_join(cc_zips_center, cc_representatives, by = 'CounDist')
 
##Intersection of Council Districts and Zip Codes
cc_intersection <- st_intersection(x = ny_zips_drivers, y = st_buffer(cc_shp, -100))
cc_intersection_df <- as.data.frame(cc_intersection) %>% select(c("ZIPCODE", "CounDist"))
cc_intersection_df <- cc_intersection_df %>% group_by(CounDist) %>% mutate(id = row_number()) %>% pivot_wider(names_from = 'id', values_from = 'ZIPCODE')
cc_intersection_df$Zips_represented <- apply(cc_intersection_df[ , 2:50] , 1 , paste , collapse = ", " )
cc_intersection_df$Zips_represented <- str_replace_all(cc_intersection_df$Zips_represented, "NA", "")

#Need to run 9 times to remove all trailing ","
for (i in 1:50){
  cc_intersection_df$Zips_represented <- gsub(", $", "", cc_intersection_df$Zips_represented)
}
cc_and_zips_df2 <- cc_intersection_df %>% select("CounDist", "Zips_represented")
cc_representatives$CounDist <- as.numeric(cc_representatives$CounDist)
cc_zips_reps3 <- left_join(cc_representatives, cc_and_zips_df2, by = "CounDist")
cc_zips_reps3$CounDist <- as.character(cc_zips_reps3$CounDist)


################
bins <- seq(0, 5000, by = 1000)
densities <- colorBin("RdPu", domain = nyc_zips_transform$Total, bins = bins)


#shiny ui ----
ui <- dashboardPage( title = "driverzipsmap",
                      dashboardHeader(title = "Map of Drvier Residences"),
                      dashboardSidebar(
                        tags$style(HTML("
                                          .main-sidebar{
                                              width: 300px;
                                             
                                            }
                                            
                                            label{color: dimgrey;}
                                            h3{
                                            padding-left: 15px; padding-right: 15px; color: dimgrey; 
                                            
                                            }
                                            
                                            h4 { 
                                            padding-left: 15px; padding-right: 15px; color: dimgrey; 
                                            }
                                            
                                            
                                            .table {
                                            padding-left: 15px;
                                            padding-right: 15px;
                                          font-size:15px;
                                          color: dimgrey;
                                          font-weight: 800;
                                          margin-bottom: 0.3rem;
                                          margin-top: 0.3rem
                                      } ")),
                        
                        h4("Select which set of district boundaries to view:"),
                        
                        selectInput('districts', 'Boundaries:', choices = c('City Council', 'State Assembly', 'State Senate', 'Congressional District'), selected = 1),
                        tableOutput("table1"),
                        tableOutput("table2")),
                     
                          dashboardBody(
                        
                          tags$head(tags$style(HTML('  .skin-blue    .main-sidebar{        
                                                                     width: 300px;
                                                                      background-color: #fdf3f3;   }      
                              
                              .main-header > .navbar {        
                                            margin-left: 300px;      }      
                              
                              .main-header .logo {         
                                            width: 300px;      }      
                              
                              .content-wrapper, .main-footer, .right-side { 
                                                                  margin-left: 300px;
                                                                  background-color: #e7eaf6 }    '))),
                          leafletOutput("mymap", height = 710)))


#Shiny App ----
##Fluid Page Shiny ----







##Server Shiny ----
server <- function(input, output, session) {
  

  
 
  
  input_district <- reactive({

    if (input$districts == 'City Council'){ 
     
      cc_shp <- read_sf('data/nycc_22a/nycc.shp')
      names(cc_shp)[names(cc_shp) == 'CounDist'] <- 'district'
      cc_shp$district <- as.numeric(cc_shp$district)
      cc_transform <- st_transform(cc_shp, "+init=epsg:4326")
      
      
    } else {
        if (input$districts == 'State Assembly') {
          
          ad_shp <- read_sf('data/nyad_22a/nyad.shp')
          names(ad_shp)[names(ad_shp) == 'AssemDist'] <- 'district'
          ad_shp$district <- as.numeric(ad_shp$district)
          ad_transform <- st_transform(ad_shp, "+init=epsg:4326")
          
          
        } else{
            if (input$districts == 'State Senate') {
              
              ss_shp <- read_sf('data/nyss_22a1/nyss.shp')
              names(ss_shp)[names(ss_shp) == 'StSenDist'] <- 'district'
              ss_shp$district <- as.numeric(ss_shp$district)
              ss_transform <- st_transform(ss_shp, "+init=epsg:4326")
              
              
            } else{
              if (input$districts == 'Congressional District') {
                
                cd_shp <- read_sf('data/nycg_22a/nycg.shp')
                names(cd_shp)[names(cd_shp) == 'CongDist'] <- 'district'
                cd_shp$district <- as.numeric(cd_shp$district)
                cd_transform <- st_transform(cd_shp, "+init=epsg:4326")
                
              }
            }
          }
        }
      })
  
 


  output$mymap <- renderLeaflet({
    
    
     leaflet() %>% addProviderTiles("OpenStreetMap") %>% 
      addPolygons(data = nyc_zips_transform, 
                  weight = 1.6, 
                  fillColor = ~densities(nyc_zips_transform$Total), 
                  color = 'grey', 
                  fillOpacity = 0.7, 
                  opacity = 0.9,
                  # We must include group so we can switch between them.
                  group = 'Zips',
                  layerId = ~ZIPCODE,
                  # label = ~ZIPCODE,
                  #options = list(zIndex = 4),
                  popup = paste0("<b>", "Zip Code: ", "</b>", ny_zips_drivers$ZIPCODE, "<br>",
                                 "<b>", "Total Drivers: ", "</b>", ny_zips_drivers$Total),
                  highlightOptions = highlightOptions(weight = 4,
                                                      color = "white"
                  ))%>% 
    
    addPolygons(data = input_district(), 
                                                                     #fillColor = transparent,
                                                                     color = 'black', 
                                                                     weight = 2.5, 
                                                                     opacity = 3,
                                                                     fillOpacity = 0,
                                                                     group = 'District', 
                                                                    label = input_district(),
                                                                     options = pathOptions(clickable = FALSE),
                                                                     
                                                                  
                                                                  ) %>% 
      # addLabelOnlyMarkers(data = input_airport(),
      #                     label = ~CongDist,
      #                     labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>% 
                                                        
      # addLayersControl(baseGroups =c("District"), overlayGroups = c('Zip Codes'),
      #                  options = layersControlOptions(collapsed=FALSE)) %>% 
      addLegend(pal = densities, 
                values = nyc_zips_transform$Total, 
                opacity = 0.8, 
                title = NULL,
                position = "bottomright") %>% 
       
      
      htmlwidgets::onRender("
                                  function(el, x) {
                                    this.on('baselayerchange', function(e) {
                                      e.layer.bringToBack();
                                    })
                                  }
                                ")
     
  })
  
  
  observeEvent(input$mymap_shape_click, {
    click <- input$mymap_shape_click
    print(click$id)
    zip_selected <- nyc_zips_transform %>% filter(ZIPCODE == click$id)
    #updateSelectInput(session, "stations", "Click on Zip Code", 
                     # choices = levels(factor(nyc_zips_transform$ZIPCODE)), 
                      #selected = c(input$stations, zip_selected))
    
  })
  
  output$table2 <- renderTable({ 
    click <- input$mymap_shape_click
    if (input$districts == 'City Council'){ 
      if (is.null(click$id) || click$id == '0') {}
      else {#zip <- as.numeric(click$id)
        #nyc_zips_transform$ZIPCODE <- as.numeric(nyc_zips_transform$ZIPCODE)
        zip <- click$id
        print(zip)
        cc <- cc_zips_center_reps %>% filter(grepl(zip, ZIPCODE)) %>% select("CounDist", "Council Member", "Borough" )
        st_drop_geometry(cc)
      }
    } else { if (input$districts == 'State Assembly'){ 
      
      if (is.null(click$id) || click$id == '0') {}
      else {#zip <- as.numeric(click$id)
        #nyc_zips_transform$ZIPCODE <- as.numeric(nyc_zips_transform$ZIPCODE)
        zip <- click$id
        print(zip)
        ad <- ad_zips_center_reps %>% filter(grepl(zip, ZIPCODE)) %>% select("AssemDist", "Assembly Member")
        st_drop_geometry(ad)
      }
      
      
    } else {if (input$districts == 'State Senate'){
      if (is.null(click$id) || click$id == '0') {}
      else {#zip <- as.numeric(click$id)
        #nyc_zips_transform$ZIPCODE <- as.numeric(nyc_zips_transform$ZIPCODE)
        zip <- click$id
        print(zip)
        ss <- ss_zips_center_reps %>% filter(grepl(zip, ZIPCODE)) %>% select("StSenDist", "State Senator")
        st_drop_geometry(ss)
      }
    }
      else {if (input$districts == 'Congressional District') { 
        if (is.null(click$id) || click$id == '0') {}
        else {
          #zip <- as.numeric(click$id)
          #nyc_zips_transform$ZIPCODE <- as.numeric(nyc_zips_transform$ZIPCODE)
          zip <- click$id
          print(zip)
          cd <- cd_zips_center_reps %>% filter(grepl(zip, ZIPCODE))  %>% select("CongDist", "Congress Person")
          st_drop_geometry(cd)
        }
      }
      }
    }
    }
    
    
    
    
  })
  
  output$table1 <- renderTable({
    click <- input$mymap_shape_click
    
    if (input$districts == 'City Council'){ 
                  if (is.null(click$id) || click$id == '0') {print("Select a Zipcode:")}
                  else {zip <- as.numeric(click$id)
                        nyc_zips_transform$ZIPCODE <- as.numeric(nyc_zips_transform$ZIPCODE)
                        
                        cc_zips_reps3 %>% filter(grepl(zip, Zips_represented)) %>% select("CounDist", "Council Member", "Borough" )}
    } else { if (input$districts == 'State Assembly'){ 
      
                    if (is.null(click$id) || click$id == '0') {print("Select a Zipcode:")}
                    else {zip <- as.numeric(click$id)
                    nyc_zips_transform$ZIPCODE <- as.numeric(nyc_zips_transform$ZIPCODE)
                    
                    ad_zips_reps2 %>% filter(grepl(zip, Zips_represented)) %>% select("AssemDist", "Assembly Member")
                 
                    }
                     
       
    } else {if (input$districts == 'State Senate'){
                      if (is.null(click$id) || click$id == '0') {print("Select a Zipcode:")}
                      else {zip <- as.numeric(click$id)
                      nyc_zips_transform$ZIPCODE <- as.numeric(nyc_zips_transform$ZIPCODE)
                      
                      ss_zips_reps2 %>% filter(grepl(zip, Zips_represented)) %>% select("StSenDist", "State Senator")
                     }
      }
      else {if (input$districts == 'Congressional District') { 
                      if (is.null(click$id) || click$id == '0') {print("Select a Zipcode:")}
                      else {
                       zip <- as.numeric(click$id)
                      nyc_zips_transform$ZIPCODE <- as.numeric(nyc_zips_transform$ZIPCODE)
                      
                      cd_zips_reps2 %>% filter(grepl(zip, Zips_represented))  %>% select("CongDist", "Congress Person")
                      }
      }
      }
        }
      }
    })
  
  
  
  
  
  
  
  
}

#Run Shiny ----
shinyApp(ui, server)

