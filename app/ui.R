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
                 src = "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Normal_Distribution_PDF.svg/1280px-Normal_Distribution_PDF.svg.png",
                 width = "100%",
                 style = "opacity: 0.90"
               ),
               fluidRow(
                 absolutePanel(
                   #style = "",
                   top = "20%",
                   left = "25%",
                   right = "25%",
                   height = 170,
                   tags$p(
                     style = "padding: 5%; background-color: yellow; font-family: alegreya; font-size: 120%;opacity: 0.50",
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
                                        tags$h4('title1'), 
                                        tags$br(),
                                        tags$h5('title2'), 
                                        prettyRadioButtons(
                                          inputId = "Id1",
                                          label = "Label1:", 
                                          choices = c("1", 
                                                      "2", 
                                                      "3",
                                                      "4"),
                                          inline = TRUE, 
                                          status = "danger",
                                          fill = TRUE
                                        ),
                                        awesomeRadio("Id2", 
                                                     label="Label2",
                                                     choices =c("a",
                                                                "b", 
                                                                "c"), 
                                                     selected = "b",
                                                     status = "warning"),
                                        selectInput('id3',
                                                    label = 'Label3',
                                                    choices = c('Yes','No','?'),
                                                    selected = '?'
                                        ),
                                        sliderInput(inputId = "radiu",
                                                    label = "radiu:",
                                                    min = 1,
                                                    max = 15,
                                                    value = 10),
                                        style = "opacity: 0.80"
                                        
                          ), #Panel Control - Closing
                      ) #Maps - Div closing
             ), #tabPanel maps closing
                  
            #------------------------------- tab panel - 3 ---------------------------------
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
              "plots",
              icon=icon("fire-alt"),
              # Main panel for displaying outputs ----
              mainPanel(
                # Output: Histogram ----
                plotOutput(outputId = "distPlot_5"),
                plotOutput(outputId = "distPlot_6")
              )
              
            ),             
            
            #------------------------------- tab panel - 5 ---------------------------------
            tabPanel(
              "Panel5",
              icon=icon("fire-alt"),
            ),             
             
  ) #navbarPage closing  
) #Shiny UI closing    
