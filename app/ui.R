library(shiny)
library(shinyWidgets) 
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(DT)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(strong("311 Study",style="color: black;"), 
             theme=shinytheme("lumen"), # from https://rstudio.github.io/shinythemes/
             #------------------------------- tab panel - Dashboard ---------------------------------
             tabPanel(
               "Introduction",
               icon=icon("fire-alt"),
               tags$img(
                 ## need to change
                 src = "https://pbs.twimg.com/media/ER8s5YYXkAIwh78.jpg",
                 width = "100%",
                 style = "opacity: 0.90"
               ),
               fluidRow(
                 absolutePanel(
                   #style = "",
                   top = "20%",
                   left = "5%",
                   right = "52%",
                   height = 170,
                   tags$p(
                     style = "padding: 5%; background-color: white; font-family: alegreya; font-size: 120%;opacity: 0.90",
                     "Some introduction!!!" 
                   )
                 )
               )
             ),
            
             #------------------------------- tab panel - Maps ---------------------------------
             tabPanel("Maps",
                      icon = icon("map-marker-alt"), #choose the icon for
                      div(class = 'outer',
                          # side by side plots
                          fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"), 
                                        leafletOutput("left_map",width="100%",height=800),
                                        leafletOutput("right_map",width="100%",height=800))),
                          #control panel on the left
                          absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                        top = 200, left = 50, right = "auto", bottom = "auto", width = 250, height = "auto",
                                        tags$h4('Left Map'), 
                                        
                                        selectInput('phase1',
                                                    label = '',
                                                    choices = c('Phase 0: Oct 2019 - Feb 2020','Phase 1: Mar 2020 - May 2020',
                                                                'Phase 2: Jun 2020 - Oct 2020','Phase 3: Nov 2020 - May 2021',
                                                                'Phase 4: Jun 2021 - Oct 2021','Phase 5: Nov 2021 - present',
                                                                "Overall", "Population"),
                                                    selected = 'Phase 5: Nov 2021 - present'),
                                        
                                        checkboxInput('norm_p1',label="per 10000 people",value = TRUE ) ,
                                        checkboxInput('norm_d1',label="per day",value = TRUE ),
                                        checkboxInput('covid1',label="Only covid-related",value = FALSE ),
                                        tags$br(),
                                        tags$h4('Right Map'), 
                                        selectInput('phase2',
                                                    label = '',
                                                    choices = c('Phase 0: Oct 2019 - Feb 2020','Phase 1: Mar 2020 - May 2020',
                                                                'Phase 2: Jun 2020 - Oct 2020','Phase 3: Nov 2020 - May 2021',
                                                                'Phase 4: Jun 2021 - Oct 2021','Phase 5: Nov 2021 - present',
                                                                "Overall", "Population"),
                                                    selected = 'Phase 5: Nov 2021 - present'),
                                        checkboxInput('norm_p2',label="per 10000 people",value = TRUE ),
                                        checkboxInput('norm_d2',label="per day",value = TRUE ),
                                        checkboxInput('covid2',label="Only covid-related",value = TRUE ),

                                        style = "opacity: 0.80"
                                        
                          ), #Panel Control - Closing
                      ) #Maps - Div closing
             ), #tabPanel maps closing
                
            #------------------------------- tab panel - 3 plots ---------------------------------
            tabPanel(
              "Covid-19 calls throughout the pandemic",
              icon=icon("fire-alt"),
              # Main panel for displaying outputs ----
              mainPanel(
                # Output
                plotOutput(outputId = "Barchart1"),
                plotOutput(outputId = "ts"),
                selectInput('complaint_type',
                            label = 'Complaint Type',
                            choices = c("noncompliance with phased reopening",
                                        "covid-19 non-essential construction",
                                        "mass gathering complaint",
                                        "vaccine mandate non-compliance",
                                        "face covering violation"
                            ),
                            selected = "noncompliance with phased reopening"),
                plotOutput(outputId = "Barchart2")
              )
            ),          
            #------------------------------- tab panel - Correlation ---------------------------------
            tabPanel(
              "Correlation between Covid Calls and Non-Covid Calls",
              icon=icon("fire-alt"),
              # Main panel for displaying outputs ----
              mainPanel(
                # Output
                plotOutput(outputId = "Correlation"),
                tags$h4("-----------------------------------------------------"),
                dataTableOutput(outputId = "Datatable"),
              )
            ),  
            
            #------------------------------- tab panel - network diagram ---------------------------------
            tabPanel(
              "Covid cases/wordcloud",
              icon=icon("fire-alt"),
              sidebarPanel(
                width = 3,
                tags$h4('Pandemic Phases: '), 
                
                selectInput('phase',
                            label = '',
                            choices = c('Phase 0: Oct 2019 - Feb 2020','Phase 1: Mar 2020 - May 2020',
                                        'Phase 2: Jun 2020 - Oct 2020','Phase 3: Nov 2020 - May 2021',
                                        'Phase 4: Jun 2021 - Oct 2021','Phase 5: Nov 2021 - present',
                                        "Overall"),
                            selected = 'Phase 0: Oct 2019 - Feb 2020'),
                
                prettyRadioButtons(
                  inputId = "cum",
                  label = "", 
                  choices = c("7-day avg","Cumulative"),
                  inline = TRUE, 
                  status = "danger",
                  fill = TRUE,
                  selected =  "7-day avg"
                ) 
                
              ),
              mainPanel(
                h2(textOutput("phase_text")),
                plotOutput(outputId = "covid_cases"),
                plotOutput(outputId = "distPlot_network")
              ),
            ),           
            #------------------------------- tab panel - Conclusion ---------------------------------
            tabPanel(
              "Conclusion ",
              icon=icon("fire-alt"),
              # Main panel for displaying outputs ----
              mainPanel(
                # Output
                 tags$h2("References"),
                 tags$h4("1. abc"),
                 tags$h4("2. abc"),
                 tags$h2("Appendix"),
              )
            ),
             
  ) #navbarPage closing  
) #Shiny UI closing    
