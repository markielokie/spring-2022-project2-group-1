library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(magrittr)
library(mapview)
library(leafsync)
library(zoo)
library(rgdal)
source("../doc/311_network_diagrams.R")

#Data Processing

nyczip = readOGR("../data/zip_code_040114.geojson")
zip_pop=read_csv("../data/nyc_zip_borough_neighborhoods_pop.csv")
pop=zip_pop$population[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==zip_pop$zip)}))]
pop[is.na(pop)] = 0
nyczip$POPULATION=pop

map1=leaflet(nyczip) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron" )
map2=leaflet(nyczip) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron" )
pal <- colorNumeric("Greens", NULL)
# ------- the date should be changed 
#time1 = brief311[difftime(brief311$`Created Date`,"2022-01-31") <= 0,] #2022-01-01 ~ 2022-01-31
#time2 = brief311[difftime(brief311$`Created Date`,"2022-01-31")>=0,] #2022-02-01 ~ 2022-02-09
# ----------------------------------

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ## Map Tab section
  
  #-------------------------- left map --------------------------
  
  output$left_map <- renderLeaflet({
    phase  = input$phase1
    map1%>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                  fillColor = ~pal((POPULATION)),
                  popup = ~(paste0( 
                    "<b>Zip Code: ",ZIPCODE,
                    "<br/>Population: ",POPULATION) )  )  %>%
      addLegend(pal = pal, values = ~POPULATION, opacity = 0.8) 
      
    }) #left map plot
  
  #-------------------------- right map --------------------------

    output$right_map <- renderLeaflet({
      phase  = input$phase2
      map2%>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                    fillColor = ~pal((POPULATION)),
                    popup = ~(paste0( 
                      "<b>Zip Code: ",ZIPCODE,
                      "<br/>Population: ",POPULATION) )  )  %>%
        addLegend(pal = pal, values = ~POPULATION, opacity = 0.8) 
      
    }) #right map plot
  

#-------------------------- covid_cases.R --------------------------

  # Specify source link
  urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  
  # Read latest data from source
  dat = read_csv(urlfile)
  
  output$distPlot_3 <- renderPlot({  
    
    # Filter data for New York City
    # Create 7-day moving averages for cases and deaths
    
    date1 = as.Date(paste0(input$year1,'-',input$month1,'-',input$day1))
    date2 = as.Date(paste0(input$year2,'-',input$month2,'-',input$day2))
   
    dat_ma = dat %>%
      filter(county == "New York City",date>=date1,date<=date2) %>%
      arrange(date) %>%
      mutate(daily_cases=c(1,diff(cases)),daily_deaths=c(0,diff(deaths)))%>%
      mutate(cases_7dayMA = rollmean(daily_cases, k=7, fill=NA),
             deaths_7dayMA = rollmean(daily_deaths, k=7, fill=NA)) %>%
      filter(date>=date1+3,date<=date2-3)
       
      # Plot 7-day MA for cases and deaths
      dat_ma %>%
        ggplot(aes(x=date, y=cases_7dayMA)) +
        geom_line(color="orange") +
        theme(legend.position="none") +
        geom_line(aes(x=date, y=deaths_7dayMA*50), color="red") +
        scale_y_continuous(
          labels=scales::comma,
          name="Cases",
          sec.axis=sec_axis(deaths_7dayMA~./50,
                            name="Deaths",
                            labels=scales::comma)
        ) +
        theme(
          axis.title.y=element_text(color="orange", size=13),
          axis.title.y.right=element_text(color="red", size=13),
          axis.text.x=element_text(size=6, angle=45, hjust=1),
          axis.text.y=element_text(size=6)
        ) +
        labs(
          title="Daily Cases & Deaths in NYC",
          subtitle="7-Day Moving Average",
          x="Date"
        ) 
  
    }) ## close distPlot_3
  
  
  output$distPlot_4 <- renderPlot({ 
    # Filter data for New York City
    # Cumulative Cases & Deaths in NYC
    
    date1 = as.Date(paste0(input$year1,'-',input$month1,'-',input$day1))
    date2 = as.Date(paste0(input$year2,'-',input$month2,'-',input$day2))
    
    dat_ma = dat %>%
      filter(county == "New York City",date>=date1,date<=date2) %>%
      arrange(date) %>%
      mutate(daily_cases=c(1,diff(cases)),daily_deaths=c(0,diff(deaths)))%>%
      mutate(cases_7dayMA = rollmean(daily_cases, k=7, fill=NA),
             deaths_7dayMA = rollmean(daily_deaths, k=7, fill=NA)) %>%
      filter(date>=date1+3,date<=date2-3)
    
    # Plot Cumulative Cases & Deaths in NYC
    dat_ma %>%
      ggplot(aes(x=date, y=cases)) +
      geom_line(color="orange") +
      theme(legend.position="none") +
      geom_line(aes(x=date, y=deaths*30), color="red") +
      scale_y_continuous(
        labels=scales::comma,
        name="Cases",
        sec.axis=sec_axis(deaths~./30,
                          name="Deaths",
                          labels=scales::comma)
      ) +
      theme(
        axis.title.y=element_text(color="orange", size=13),
        axis.title.y.right=element_text(color="red", size=13),
        axis.text.x=element_text(size=6, angle=45, hjust=1),
        axis.text.y=element_text(size=6)
      ) +
      labs(
        title="Cumulative Cases & Deaths in NYC",
        x="Date"
      ) 
  }) ## close distPlot_4
  
  
  
  #-------------------   distPlot_network  ------------------- 
  
  
  output$distPlot_network <- renderPlot({ 
    
    plot_network_diagram(biword_df = biwords_counts, phase_str = paste0("phase",substr(input$phase,start=7,stop=7)))
    
  }) ## close distPlot_network
  
  output$phase_text  <- renderText ({ 
    
    p=substr(input$phase,start=7,stop=7)
    if(p=="0"){p="Phase 0: pre-pandemic (Oct 2019 - Feb 2020)"}
    else if(p=="1"){p="Phase 1: initial outbreak (Mar 2020 - May 2020)"}
    else if(p=="2"){p="Phase 2: cases go down (Jun 2020 - Oct 2020)"}
    else if(p=="3"){p="Phase 3: cases go up + delta variant (Nov 2020 - May 2021)"}
    else if(p=="4"){p="Phase 4: cases go down (Jun 2021 - Oct 2021)"}
    else if(p=="5"){p="Phase 5: cases go up + omicron variant (Nov 2021 - present)"}
    return(p)
    
  }) ## close phase_text
  
})



