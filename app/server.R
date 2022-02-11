library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(magrittr)
library(mapview)
library(leafsync)


#Data Processing
brief311 = read_csv('../data/2022_brief.csv')

brief311 = brief311 %>%
  mutate(`Created Date` = as.Date(brief311$`Created Date`,format = "%m/%d/%Y"))%>%
  drop_na()


time1 = brief311[difftime(brief311$`Created Date`,"2022-01-31") <= 0,] #2022-01-01 ~ 2022-01-31
time2 = brief311[difftime(brief311$`Created Date`,"2022-01-31")>=0,] #2022-02-01 ~ 2022-02-09

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## Map Tab section
  
  output$left_map <- renderLeaflet({

    map_time1 <- time1%>% 
      group_by(Latitude, Longitude) %>%
      count()%>%
      leaflet(options = leafletOptions(minZoom = 10, maxZoom = 16)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-73.9834,40.7504,zoom = 12)
      
    map_time1 %>%
        addHeatmap(
          lng=~Longitude,
          lat=~Latitude,
          intensity=~n,#change to total day diff percentage
          max=10,
          radius=8,
          blur=10)
      
    }) #left map plot
  
  output$right_map <- renderLeaflet({


    
    map_time2 <- time2%>% 
      group_by(Latitude, Longitude) %>%
      count()%>%
      leaflet(options = leafletOptions(minZoom = 10, maxZoom = 16)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-73.9834,40.7504,zoom = 12)
    
    map_time2 %>%
      addHeatmap(
        lng=~Longitude,
        lat=~Latitude,
        intensity=~n,#change to total day diff percentage
        max=10,
        radius=8,
        blur=10)
    
    
    
  }) #right map plot

})


