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
source("lib/311_network_diagrams.R")
source("lib/covid_plots.R")
source("lib/cleaning.R")
source("lib/plots.R")
#Data Processing

## New York City's geojson Data
nyczip = readOGR("data/zip_code_040114.geojson")

## Population for each zip code
zip_pop=read_csv("data/nyc_zip_borough_neighborhoods_pop.csv")
pop=zip_pop$population[as.numeric(mapply(nyczip$ZIPCODE,FUN = function(x){which (x==zip_pop$zip)}))]
pop[is.na(pop)] = 0
nyczip$POPULATION=pop

## Number of calls in each phase group by zip code
ncalls =  read_csv("output/ncall_phase.csv")
ncalls_covid = read_csv("output/ncall_phase_covid.csv")

## COVID cases
covid_data = read_csv("output/covid_cases.csv")%>%
  filter(date>=as.Date(paste0(2019,'-',10,'-',1)),date<=as.Date(paste0(2022,'-',2,'-',11)))

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
covid_calls = read.csv("output/covid_calls.csv", na.strings=c("","N/A","NA"))
ncalls = read.csv("output/ncalls_by_date_complaint.csv", na.strings=c("","N/A","NA"))
complaint_corr = read.csv("output/complaint_corr.csv", na.strings=c("","N/A","NA"))
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

neighborhoods = read.csv("data/nyc_zip_borough_neighborhoods_pop.csv", na.strings=c("","N/A","NA"))
borough_pop = neighborhoods %>% 
  group_by(borough) %>% 
  summarise(total_pop = sum(population))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ## Map Tab section
  
  #-------------------------- left map --------------------------
  
  output$left_map <- renderLeaflet({
   
    if (input$phase1 =="Population #") {
      pal <- colorNumeric("YlOrRd",NULL)
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
      pal <- colorNumeric("YlOrRd", 0:20*lambda, na.color = "#8b0000")
      
      map1=leaflet(nyczip) %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron" ) %>%
        addPolygons(data = nyczip,stroke = TRUE,color = "white",weight = 1, fillOpacity = 0.7,
                    fillColor = ~pal((Volume)),
                    popup = ~(paste0( 
                      "<b>Zip Code: ",ZIPCODE,
                      "<br/>Population: ",POPULATION,
                      "<br/>Volume: ",round(Volume,5))),
                    highlight = highlightOptions(
                      weight = 1, color = "red",bringToFront = TRUE) 
                    )  %>%
        addLegend(pal = pal, values = 0:20*lambda, title =  "# of Calls", opacity = 0.8) 
    }
  }) #left map plot
  
  #-------------------------- right map --------------------------

    output$right_map <- renderLeaflet({
      
      if (input$phase2 =="Population #") {
        pal <- colorNumeric("YlOrRd",NULL)
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
        pal <- colorNumeric("YlOrRd", 0:20*lambda, na.color = "#8b0000")
        
        map2=leaflet(nyczip) %>%
          addTiles() %>%
          addProviderTiles("CartoDB.Positron" ) %>%
          addPolygons(data = nyczip,stroke = TRUE,color = "white",weight = 1, fillOpacity = 0.7,
                      fillColor = ~pal((Volume)),
                      popup = ~(paste0( 
                        "<b>Zip Code: ",ZIPCODE,
                        "<br/>Population: ",POPULATION,
                        "<br/>Volume: ",round(Volume,5))),
                      highlight = highlightOptions(
                        weight = 1, color = "red",bringToFront = TRUE) 
                      ) %>%
          addLegend(pal = pal, values = 0:20*lambda, title =  "# of Calls", opacity = 0.8) 
      }
    }) #right map plot
  

#-------------------------- covid_cases.R --------------------------
#-------------------------- distPlot_network  ------------------- 

  output$phase_text  <- renderText ({ 
    
    p=substr(input$phase,start=7,stop=7)
    if(p=="0"){p="Phase 0: Pre-Pandemic (Oct 2019 - Feb 2020)"}
    else if(p=="1"){p="Phase 1: Initial Outbreak (Mar 2020 - May 2020)"}
    else if(p=="2"){p="Phase 2: Cases Drop (Jun 2020 - Oct 2020)"}
    else if(p=="3"){p="Phase 3: Cases Rise + Delta Variant (Nov 2020 - May 2021)"}
    else if(p=="4"){p="Phase 4: Cases Drop (Jun 2021 - Oct 2021)"}
    else if(p=="5"){p="Phase 5: Cases Rise + Omicron Variant (Nov 2021 - present)"}
    else {p="Overall"}
    return(p)
    
  }) ## close phase_text    
  
  
  output$phase_text2  <- renderText ({ 
    
    p="The network diagram shows the interconnections between words that represent the topics in the 311 calls. The thicker the connections between words, the greater the frequency of occurrence. The top 60 word connections are shown and the frequency of occurrence ranges from tens of thousands to hundreds of thousands (i.e., 10,000+ to 100,000+)."
    return(p)
    
  }) ## close phase_text    
 
  
  output$phase_text3  <- renderText ({ 
    
    p=substr(input$phase,start=7,stop=7)
    if(p=="0"){p='1. We observe that "residential building" issues dominate the 311 calls, with many pertaining to "hot water" issues in the building and "noisy neighbors". \n
2. We also notice high volume of payment-related issues like "property tax", "payment disputes", "camera violations" and "parking tickets".\n
3. There are high number of calls on "bulk items" collection and the data suggest that people call 311 for scheduling, rescheduling and cancellation of appointments.'}
    else if(p=="1"){p='1. Expectedly, "Covid-19 pandemic" issues dominate the calls amidst fears of the novel coronavirus. \n
2. Calls pertaining to "consumers", "workers" and "families" were alluding to fears over business closures and job security.\n
3. Many people were calling to inquire about "social distancing guidelines" and "symptoms/prevention" as well.\n
4. Issues that were dominant before the pandemic were overshadowed by Covid-related calls.'}
    else if(p=="2"){p='1. While "Covid-19" issues continue to dominate majority of the calls, other issues start to increase in volume commensurate with Covid-19 calls as with during the pre-pandemic phase.\n
2. Notably, "afford delivery food" and "low income seniors" start to appear in the calls, which could imply the increase in the need for financial assistance.'}
    else if(p=="3"){p='1.With the emergence of the Delta variant and the successful approval of the mRNA vaccine for emergency use, calls relating to the "Covid-19 vaccine" started to appear for the first time.\n
2. Also, because of Covid, applying for IDNYC cards were only available by appointments and on limited basis. This could explain the increase in calls pertaining to "idnyc municipal identification cards".\n
3. The usual "bulk item", "camera violations", "payment disputes", "parking tickets" and "hot water" issues start to reappear as with the pre-pandemic phase. '}
    else if(p=="4"){p='1. When the number of cases when down, the call volume relating to "Covid-19" dropped as well. \n
2. Calls relating to "residential", "camera violations", "property tax" and "idnyc municipal identification card" appointments continue to dominate the calls.'}
    else if(p=="5"){p='1. The occurrence of calls relating to the "Covid-19 vaccine" started to resurface and this is likely due to the increased fears in the Omicron variant. To note, "symptoms prevention" and \n
2. Calls relating to "residential buildings", "camera violations"and "property tax" issues continue to dominate the calls.'}
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
    covid_plot(covid_data,date1,date2,input$cum=="Cumulative")
  
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
    plot.borough.bp(covid_calls,borough_pop, complaint)
    
  })
  
  output$Correlation <- renderPlot({  
    
    plot.corr(complaint_corr)
    
  })
  output$Datatable <- renderDataTable({  
    
    datatable(complaint_corr, 
              options = list(pageLength=10, 
                             initComplete=JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'color': '#fff'});",
                               "}"
                             ))) %>%
      formatRound(columns = colnames(complaint_corr), digits=3)
    
  })
  
})



