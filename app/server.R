library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(magrittr)
library(mapview)
library(leafsync)
library(zoo)
library(rgdal)
library(DT)
library(pheatmap)
library(ggplot2)
library(stringr)
source("../doc/311_network_diagrams.R")
source("../lib/covid_plots.R")
source("../lib/cleaning.R")
source("../lib/plots.R")

#Data Processing

## New York City's geojson Data
nyczip = readOGR("../data/zip_code_040114.geojson")

## Population for each zip code
zip_pop=read_csv("../data/nyc_zip_borough_neighborhoods_pop.csv")
pop=zip_pop$population[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==zip_pop$zip)}))]
pop[is.na(pop)] = 0
nyczip$POPULATION=pop

## Number of calls in each phase group by zip code
ncalls =  read_csv("../output/ncall_phase.csv")
ncalls_covid = read_csv("../output/ncall_phase_covid.csv")

### --------------------- All calls -------------------
phase0=ncalls$phase0[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls$Incident.Zip)}))]
phase0[is.na(phase0)] = 0
nyczip$phase0=phase0

phase1=ncalls$phase1[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls$Incident.Zip)}))]
phase1[is.na(phase1)] = 0
nyczip$phase1=phase1

phase2=ncalls$phase2[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls$Incident.Zip)}))]
phase2[is.na(phase2)] = 0
nyczip$phase2=phase2

phase3=ncalls$phase3[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls$Incident.Zip)}))]
phase3[is.na(phase3)] = 0
nyczip$phase3=phase3

phase4=ncalls$phase4[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls$Incident.Zip)}))]
phase4[is.na(phase4)] = 0
nyczip$phase4=phase4

phase5=ncalls$phase5[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls$Incident.Zip)}))]
phase5[is.na(phase5)] = 0
nyczip$phase5=phase5

nyczip$all_calls=nyczip$phase0+nyczip$phase1+nyczip$phase2+nyczip$phase3+nyczip$phase4+nyczip$phase5


# ----------------------- only covid calls -------------------------------

phase0_covid=ncalls_covid$phase0[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls_covid$Incident.Zip)}))]
phase0_covid[is.na(phase0_covid)] = 0
nyczip$phase0_covid=phase0_covid

phase1_covid=ncalls_covid$phase1[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls_covid$Incident.Zip)}))]
phase1_covid[is.na(phase1_covid)] = 0
nyczip$phase1_covid=phase1_covid

phase2_covid=ncalls_covid$phase2[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls_covid$Incident.Zip)}))]
phase2_covid[is.na(phase2_covid)] = 0
nyczip$phase2_covid=phase2_covid

phase3_covid=ncalls_covid$phase3[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls_covid$Incident.Zip)}))]
phase3_covid[is.na(phase3_covid)] = 0
nyczip$phase3_covid=phase3_covid

phase4_covid=ncalls_covid$phase4[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls_covid$Incident.Zip)}))]
phase4_covid[is.na(phase4_covid)] = 0
nyczip$phase4_covid=phase4_covid

phase5_covid=ncalls_covid$phase5[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==ncalls_covid$Incident.Zip)}))]
phase5_covid[is.na(phase5_covid)] = 0
nyczip$phase5_covid=phase5_covid

nyczip$all_calls_covid=nyczip$phase0_covid+nyczip$phase1_covid+nyczip$phase2_covid+nyczip$phase3_covid+
  nyczip$phase4_covid+nyczip$phase5_covid

# -------------------- 3 plots ------------------------------

###  Load Data
covid_calls = read.csv("../output/covid_calls.csv", na.strings=c("","N/A","NA"))
ncalls = read.csv("../output/ncalls_by_date_complaint.csv", na.strings=c("","N/A","NA"))
complaint_corr = read.csv("../output/complaint_corr.csv", na.strings=c("","N/A","NA"))
complaint_corr = complaint_corr %>%
  remove_rownames %>% column_to_rownames(var="X")%>%
  remove_rownames %>% column_to_rownames(var="term")
colnames(complaint_corr) = c("non-compliance\nw/phased\nreopening",
                             "vaccine mandate\nnon-compliance",
                             "mass gathering\ncomplaint",
                             "covid-19\nnon-essential\nconstruction",
                             "face covering\nviolation")

covid_complaints = unique(covid_calls$Complaint.Type)
by_covid_complaint = process.freq.df(covid_calls)

neighborhood = read.csv("../data/nyc_zip_borough_neighborhoods_pop.csv", na.strings=c("","N/A","NA"))
borough_pop = neighborhoods %>% 
  group_by(borough) %>% 
  summarise(total_pop = sum(population))




















# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ## Map Tab section
  
  #-------------------------- left map --------------------------
  
  output$left_map <- renderLeaflet({
   
    if (input$phase1 =="Population") {
      pal <- colorNumeric("Greens",NULL)
      map1=leaflet(nyczip) %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron" ) %>%
        addPolygons(stroke = TRUE,color = "white",weight = 1, fillOpacity = 0.7,
                    fillColor = ~pal((POPULATION)),
                    popup = ~(paste0( 
                      "<b>Zip Code: ",ZIPCODE,
                      "<br/>Population: ",POPULATION) ),
                    highlight = highlightOptions(
                      weight = 1, color = "red",bringToFront = TRUE) 
                    )  %>%
        addLegend(pal = pal, values = ~POPULATION, opacity = 0.8) 
    }
    else {
      p=substr(input$phase1,start=7,stop=7)
      
      if (input$covid1) {
        if(p=="0")     {vol = nyczip$phase0_covid/ifelse(input$norm_d1,151,1) }
        else if(p=="1"){vol = nyczip$phase1_covid/ifelse(input$norm_d1,90,1)}
        else if(p=="2"){vol = nyczip$phase2_covid/ifelse(input$norm_d1,152,1)}
        else if(p=="3"){vol = nyczip$phase3_covid/ifelse(input$norm_d1,211,1)}
        else if(p=="4"){vol = nyczip$phase4_covid/ifelse(input$norm_d1,152,1)}
        else if(p=="5"){vol = nyczip$phase5_covid/ifelse(input$norm_d1,102,1)}
        else           {vol = nyczip$all_calls_covid/ifelse(input$norm_d1,864,1)}
      }
      else{
        if(p=="0")     {vol = nyczip$phase0/ifelse(input$norm_d1,151,1) }
        else if(p=="1"){vol = nyczip$phase1/ifelse(input$norm_d1,90,1)}
        else if(p=="2"){vol = nyczip$phase2/ifelse(input$norm_d1,152,1)}
        else if(p=="3"){vol = nyczip$phase3/ifelse(input$norm_d1,211,1)}
        else if(p=="4"){vol = nyczip$phase4/ifelse(input$norm_d1,152,1)}
        else if(p=="5"){vol = nyczip$phase5/ifelse(input$norm_d1,102,1)}
        else           {vol = nyczip$all_calls/ifelse(input$norm_d1,864,1)}
      }
      nyczip$Volume = vol
      if(input$norm_p1){
        nyczip$Volume=nyczip$Volume/ifelse(nyczip$POPULATION==0,Inf,nyczip$POPULATION/10000)
      }
      
      ## Legend range
      lambda=ifelse(input$norm_p1,1,5)*ifelse(input$norm_d1,1,100*ifelse(input$phase1=="Overall",6,1))/
        ifelse(input$covid1,100,1)
      ## function for select color
      pal <- colorNumeric("Greens", 0:20*lambda,na.color = "#0B5345")
      
      map1=leaflet(nyczip) %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron" ) %>%
        addPolygons(data = nyczip,stroke = TRUE,color = "white",weight = 1, fillOpacity = 0.7,
                    fillColor = ~pal((Volume)),
                    popup = ~(paste0( 
                      "<b>Zip Code: ",ZIPCODE,
                      "<br/>Population: ",POPULATION,
                      "<br/>Volume:",round(Volume,5))),
                    highlight = highlightOptions(
                      weight = 1, color = "red",bringToFront = TRUE) 
                    )  %>%
        addLegend(pal = pal, values = 0:20*lambda, title =  "Calls", opacity = 0.8) 
    }
  }) #left map plot
  
  #-------------------------- right map --------------------------

    output$right_map <- renderLeaflet({
      
      if (input$phase2 =="Population") {
        pal <- colorNumeric("Greens",NULL)
        map1=leaflet(nyczip) %>%
          addTiles() %>%
          addProviderTiles("CartoDB.Positron" ) %>%
          addPolygons(stroke = TRUE,color = "white",weight = 1, fillOpacity = 0.7,
                      fillColor = ~pal((POPULATION)),
                      popup = ~(paste0( 
                        "<b>Zip Code: ",ZIPCODE,
                        "<br/>Population: ",POPULATION) ),
                      highlight = highlightOptions(
                        weight = 1, color = "red",bringToFront = TRUE) 
                      )  %>%
          addLegend(pal = pal, values = ~POPULATION, opacity = 0.8) 
      }
      else {
        p=substr(input$phase2,start=7,stop=7)
        
        if (input$covid2) {
          if(p=="0")     {vol = nyczip$phase0_covid/ifelse(input$norm_d2,151,1) }
          else if(p=="1"){vol = nyczip$phase1_covid/ifelse(input$norm_d2,90,1)}
          else if(p=="2"){vol = nyczip$phase2_covid/ifelse(input$norm_d2,152,1)}
          else if(p=="3"){vol = nyczip$phase3_covid/ifelse(input$norm_d2,211,1)}
          else if(p=="4"){vol = nyczip$phase4_covid/ifelse(input$norm_d2,152,1)}
          else if(p=="5"){vol = nyczip$phase5_covid/ifelse(input$norm_d2,102,1)}
          else           {vol = nyczip$all_calls_covid/ifelse(input$norm_d2,864,1)}
        }
        else{
          if(p=="0")     {vol = nyczip$phase0/ifelse(input$norm_d2,151,1) }
          else if(p=="1"){vol = nyczip$phase1/ifelse(input$norm_d2,90,1)}
          else if(p=="2"){vol = nyczip$phase2/ifelse(input$norm_d2,152,1)}
          else if(p=="3"){vol = nyczip$phase3/ifelse(input$norm_d2,211,1)}
          else if(p=="4"){vol = nyczip$phase4/ifelse(input$norm_d2,152,1)}
          else if(p=="5"){vol = nyczip$phase5/ifelse(input$norm_d2,102,1)}
          else           {vol = nyczip$all_calls/ifelse(input$norm_d2,864,1)}
        }
        nyczip$Volume = vol
        if(input$norm_p2){
          nyczip$Volume=nyczip$Volume/ifelse(nyczip$POPULATION==0,Inf,nyczip$POPULATION/10000)
        }
        
        ## Legend range
        lambda=ifelse(input$norm_p2,1,5)*ifelse(input$norm_d2,1,100*ifelse(input$phase2=="Overall",6,1))/
          ifelse(input$covid2,100,1)
        ## function for select color
        pal <- colorNumeric("Greens", 0:20*lambda,na.color = "#0B5345")
        
        map2=leaflet(nyczip) %>%
          addTiles() %>%
          addProviderTiles("CartoDB.Positron" ) %>%
          addPolygons(data = nyczip,stroke = TRUE,color = "white",weight = 1, fillOpacity = 0.7,
                      fillColor = ~pal((Volume)),
                      popup = ~(paste0( 
                        "<b>Zip Code: ",ZIPCODE,
                        "<br/>Population: ",POPULATION,
                        "<br/>Volume:",round(Volume,5))),
                      highlight = highlightOptions(
                        weight = 1, color = "red",bringToFront = TRUE) 
                      ) %>%
          addLegend(pal = pal, values = 0:20*lambda, title =  "Calls", opacity = 0.8) 
      }
    }) #right map plot
  

#-------------------------- covid_cases.R --------------------------
#-------------------------- distPlot_network  ------------------- 

  output$phase_text  <- renderText ({ 
    
    p=substr(input$phase,start=7,stop=7)
    if(p=="0"){p="Phase 0: pre-pandemic (Oct 2019 - Feb 2020)"}
    else if(p=="1"){p="Phase 1: initial outbreak (Mar 2020 - May 2020)"}
    else if(p=="2"){p="Phase 2: cases go down (Jun 2020 - Oct 2020)"}
    else if(p=="3"){p="Phase 3: cases go up + delta variant (Nov 2020 - May 2021)"}
    else if(p=="4"){p="Phase 4: cases go down (Jun 2021 - Oct 2021)"}
    else if(p=="5"){p="Phase 5: cases go up + omicron variant (Nov 2021 - present)"}
    else {p="Overall"}
    return(p)
    
  }) ## close phase_text    
  
  output$covid_cases <- renderPlot({  
    
    # Filter data for New York City
    # Create 7-day moving averages for cases and deaths
    
    p=substr(input$phase,start=7,stop=7)
    if(p=="0"){
    date1 = as.Date(paste0(2019,'-',10,'-',1))
    date2 = as.Date(paste0(2020,'-',2,'-',29))}
    else if(p=="1"){ 
    date1 = as.Date(paste0(2020,'-',3,'-',1))
    date2 = as.Date(paste0(2020,'-',5,'-',30))}
    else if(p=="2"){ 
    date1 = as.Date(paste0(2020,'-',6,'-',1))
    date2 = as.Date(paste0(2020,'-',10,'-',31))}
    else if(p=="3"){ 
    date1 = as.Date(paste0(2020,'-',11,'-',1))
    date2 = as.Date(paste0(2021,'-',5,'-',31))}
    else if(p=="4"){ 
    date1 = as.Date(paste0(2021,'-',6,'-',1))
    date2 = as.Date(paste0(2021,'-',10,'-',31))}
    else if(p=="5"){ 
    date1 = as.Date(paste0(2021,'-',11,'-',1))
    date2 = as.Date(paste0(2022,'-',2,'-',11))}
    else{
      date1 = as.Date(paste0(2020,'-',3,'-',1))
      date2 = as.Date(paste0(2022,'-',2,'-',11))
    }
    covid_plot(date1,date2,input$cum=="Cumulative")
  
    }) ## close covid_cases
  
  
  output$distPlot_network <- renderPlot({ 
    if(input$phase!="Overall")
    plot_network_diagram(biword_df = biwords_counts, 
                         phase_str = paste0("phase",substr(input$phase,start=7,stop=7)))

  }) ## close distPlot_network
  
  
  # -----------------------------  3 plots ---------------------------
  
  
  output$Barchart1<- renderPlot({  
    
    plot.freq.bp(by_covid_complaint)
    
  })
  output$ts <- renderPlot({  
    
    ### Time series
    plot.ts.complaints(covid_calls)
    
  })
  
  output$Barchart2 <- renderPlot({  
    
    complaint = input$complaint_type
    plot.borough.bp(covid_calls, complaint)
    
  })
  
  output$Correlation <- renderPlot({  
    
    plot.corr(complaint_corr)
    
  })
  output$Datatable <- renderDataTable({  
    
    datatable(complaint_corr, 
              options = list(pageLength=5)) %>%
      formatRound(columns = colnames(complaint_corr), digits=3)
    
  })
  
})



