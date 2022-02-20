library(shiny)
library(shinyWidgets) 
library(shinythemes)
library(leaflet)
library(leaflet.extras)


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
                 src = "https://play-lh.googleusercontent.com/frrVTacZfKCUIapOpvbtIW6BaqwwKgEyiZ_84ad4hWg1jF8I5cUirv0GLL8rFZtdCRJQ",
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
                                        leafletOutput("left_map",width="100%",height=1200),
                                        leafletOutput("right_map",width="100%",height=1200))),
                          #control panel on the left
                          absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                        top = 200, left = 50, right = "auto", bottom = "auto", width = 250, height = "auto",
                                        tags$h4('Left Map'), 
                                        
                                        selectInput('phase1',
                                                    label = 'Phase',
                                                    choices = c('Phase 0: Oct 2019 - Feb 2020','Phase 1: Mar 2020 - May 2020',
                                                                'Phase 2: Jun 2020 - Oct 2020','Phase 3: Nov 2020 - May 2021',
                                                                'Phase 4: Jun 2021 - Oct 2021','Phase 5: Nov 2021 - present'),
                                                    selected = 'Phase 0: Oct 2019 - Feb 2020'
                                        ),
                                        tags$br(),
                                        tags$h4('Right Map'), 
                                        selectInput('phase2',
                                                    label = 'Phase',
                                                    choices = c('Phase 0: Oct 2019 - Feb 2020','Phase 1: Mar 2020 - May 2020',
                                                                'Phase 2: Jun 2020 - Oct 2020','Phase 3: Nov 2020 - May 2021',
                                                                'Phase 4: Jun 2021 - Oct 2021','Phase 5: Nov 2021 - present'),
                                                    selected = 'Phase 5: Nov 2021 - present'
                                        ),

                                        style = "opacity: 0.80"
                                        
                          ), #Panel Control - Closing
                      ) #Maps - Div closing
             ), #tabPanel maps closing
                  
            #------------------------------- tab panel - Covid ---------------------------------
            tabPanel(
             "Covid Cases",
             icon=icon("fire-alt"),
             
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 width = 3,
                 tags$h4('Start Date: '), 
                
                 selectInput('year1',
                             label = 'Year',
                             choices = c('2020','2021','2022'),
                             selected = '2020'),
                 selectInput('month1',
                             label = 'Month',
                             choices = c('1','2','3','4','5','6','7','8','9','10','11','12'),
                             selected = '3'),
                 selectInput('day1',
                             label = 'Day',
                             choices = c('1','2','3','4','5','6','7','8','9','10',
                                         '11','12','13','14','15','16','17','18','19','20',
                                         '21','22','23','24','25','26','27','28','29','30','31'),
                             selected = '1'),
                 tags$h4('End Date: '),
                 selectInput('year2',
                             label = 'Year',
                             choices = c('2020','2021','2022'),
                             selected = '2022'),
                 selectInput('month2',
                             label = 'Month',
                             choices = c('1','2','3','4','5','6','7','8','9','10','11','12'),
                             selected = '2'),
                 selectInput('day2',
                             label = 'Day',
                             choices = c('1','2','3','4','5','6','7','8','9','10',
                                         '11','12','13','14','15','16','17','18','19','20',
                                         '21','22','23','24','25','26','27','28','29','30','31'),
                             selected = '8'),
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 # Output: Histogram ----
                 plotOutput(outputId = "distPlot_3"),
                 plotOutput(outputId = "distPlot_4")
               )
             )
             
            ),
            
            #------------------------------- tab panel - 4 ---------------------------------
            tabPanel(
              "panel 4",
              icon=icon("fire-alt"),
              # Main panel for displaying outputs ----
              mainPanel(
                # Output: Histogram ----
                #plotOutput(outputId = "distPlot_5"),
                #plotOutput(outputId = "distPlot_6")
              )
              
            ),             
            
            #------------------------------- tab panel - network diagram ---------------------------------
            tabPanel(
              "network diagram",
              icon=icon("fire-alt"),
              sidebarPanel(
                width = 3,
                tags$h4('Pandemic Phases: '), 
                
                selectInput('phase',
                            label = '',
                            choices = c('Phase 0: Oct 2019 - Feb 2020','Phase 1: Mar 2020 - May 2020',
                                        'Phase 2: Jun 2020 - Oct 2020','Phase 3: Nov 2020 - May 2021',
                                        'Phase 4: Jun 2021 - Oct 2021','Phase 5: Nov 2021 - present'),
                            selected = 'Phase 0: Oct 2019 - Feb 2020'),

              ),
              mainPanel(
                h2(textOutput("phase_text")),
                plotOutput(outputId = "distPlot_network")
              ),
            ),             
             
  ) #navbarPage closing  
) #Shiny UI closing    
