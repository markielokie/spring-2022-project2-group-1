library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(magrittr)
library(mapview)
library(leafsync)
library(zoo)

#Data Processing
brief311 = read_csv('../data/2022_brief.csv')

brief311 = brief311 %>%
  mutate(`Created Date` = as.Date(brief311$`Created Date`,format = "%m/%d/%Y"))%>%
  drop_na()


# ------- the date should be changed 
time1 = brief311[difftime(brief311$`Created Date`,"2022-01-31") <= 0,] #2022-01-01 ~ 2022-01-31
time2 = brief311[difftime(brief311$`Created Date`,"2022-01-31")>=0,] #2022-02-01 ~ 2022-02-09
# ----------------------------------



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ## Map Tab section
  
  #-------------------------- left map --------------------------
  
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
          radius=input$radiu,
          blur=10)
      
    }) #left map plot
  
  #-------------------------- right map --------------------------
  
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
        radius=input$radiu,
        blur=10)

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
  
  #------------------------------ plots -----------------------------
  
  
  output$distPlot_5 <- renderPlot({ 
    
    barplotdata = time1%>% ## time2 are defined at line 19 (could be changed)
      group_by(`Agency Name`) %>%
      count()%>%
      arrange(n)
    
    ggplot(data = barplotdata,aes(x =  reorder(`Agency Name`,-n)  ,y=n, fill= reorder(`Agency Name`,-n) ))+
      geom_bar(stat = "identity")+
      theme(axis.text.x = element_blank())+
      labs(x="Agency",y="Cases",fill="Agency Name",title = "Time1")
  }) ## close distPlot_5
  
  output$distPlot_6 <- renderPlot({ 
    
    barplotdata = time2%>% ## time2 are defined at line 20 (could be changed)
      group_by(`Agency Name`) %>%
      count()%>%
      arrange(n)
    
    ggplot(data = barplotdata,aes(x =  reorder(`Agency Name`,-n)  ,y=n, fill= reorder(`Agency Name`,-n) ))+
      geom_bar(stat = "identity")+
      theme(axis.text.x = element_blank())+
      labs(x="Agency",y="Cases",fill="Agency Name",title = "Time2")
  }) ## close distPlot_6
  
  
  #--------------------------------------
  
})


