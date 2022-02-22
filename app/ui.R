library(shiny)
library(shinyWidgets) 
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(DT)


shinyUI(
  navbarPage(strong("COVID-19 and 311 Calls", style="color: #FFF208;"), 
             theme=shinytheme("superhero"), # from https://rstudio.github.io/shinythemes/
             #------------------------------- tab panel - Dashboard ---------------------------------
             tabPanel(
               "Introduction",
               icon=icon("house-user"),
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
                   left = "8%",
                   right = "43%",
                   height = 170,
                   tags$p(
                     style = "padding: 5%; 
                     background-color: white; 
                     font-size: 125%; 
                     font-weight: bold;
                     opacity: 0.9;
                     color: black",
                     "Impact of COVID-19 on NYC 311 Calls"
                   ),
                   tags$p(
                     style = "padding: 5%; 
                     margin: 0%;
                     background-color: white; 
                     font-size: 90%; 
                     opacity: 0.9;
                     color: black",
                     "The COVID-19 pandemic has had an unprecedented impact on the lives of New Yorkers and on 
                     NYC government agencies. This application focuses on how the pandemic has affected NYC311,
                     the 311 service line that fields thousands of calls seeking information and assistance as 
                     well as filing complaints and reporting non-emergency violations. 311 acts as the first point 
                     of contact for any non-911 issues and redirects callers to the appropriate agency such as the 
                     Department of Buildings or the NYPD."
                   ),
                   tags$p(
                     style = "padding: 5%; 
                     margin: 0%;
                     background-color: white; 
                     font-size: 90%; 
                     opacity: 0.9;
                     color: black",
                     "Using 311 call data found on NYC OpenData, we observed changes in 311 call patterns that 
                     reflected the relevant COVID-19 issues and mandates at each stage of the pandemic. The 
                     results elicit insights that would be useful for the planning and mitigation efforts of 
                     NYC agencies. Understanding each borough’s pertinent issues in response to COVID-19 and safety 
                     mandates would facilitate more effective future resource allocation and management by the 
                     departments directly handling incidents and cases. "
                   ),
                 )
               )
             ),
            
             #------------------------------- tab panel - Maps ---------------------------------
             tabPanel("Heatmaps of call volume",
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
                                                                "Total # of Calls in all phases", "Population #"),
                                                    selected = 'Phase 5: Nov 2021 - present'),
                                        
                                        checkboxInput('norm_p1',label="Per 10,000 people",value = TRUE ) ,
                                        checkboxInput('norm_d1',label="Per day",value = TRUE ),
                                        checkboxInput('covid1',label="Only COVID-related calls",value = FALSE ),
                                        tags$br(),
                                        tags$h4('Right Map'), 
                                        selectInput('phase2',
                                                    label = '',
                                                    choices = c('Phase 0: Oct 2019 - Feb 2020','Phase 1: Mar 2020 - May 2020',
                                                                'Phase 2: Jun 2020 - Oct 2020','Phase 3: Nov 2020 - May 2021',
                                                                'Phase 4: Jun 2021 - Oct 2021','Phase 5: Nov 2021 - present',
                                                                "Total # of calls in all phases", "Population #"),
                                                    selected = 'Phase 5: Nov 2021 - present'),
                                        checkboxInput('norm_p2',label="Per 10,000 people",value = TRUE ),
                                        checkboxInput('norm_d2',label="Per day",value = TRUE ),
                                        checkboxInput('covid2',label="Only COVID-related calls",value = TRUE ),

                                        style = "opacity: 0.80"
                                        
                          ), #Panel Control - Closing
                      ) #Maps - Div closing
             ), #tabPanel maps closing
            
             #------------------------------- tab panel - network diagram ---------------------------------
             tabPanel(
               "311 call topics during the pandemic",
               icon=icon("phone"),
               sidebarPanel(
                 width = 3,
                 tags$h4('Pandemic Phases: '), 
                 
                 selectInput('phase',
                             label = '',
                             choices = c('Phase 0: Oct 2019 - Feb 2020','Phase 1: Mar 2020 - May 2020',
                                         'Phase 2: Jun 2020 - Oct 2020','Phase 3: Nov 2020 - May 2021',
                                         'Phase 4: Jun 2021 - Oct 2021','Phase 5: Nov 2021 - Present',
                                         "Overall"),
                             selected = 'Phase 0: Oct 2019 - Feb 2020'),
                 
                 prettyRadioButtons(
                   inputId = "cum",
                   label = "", 
                   choices = c("Daily 7-Day Moving Avg","Cumulative"),
                   inline = TRUE, 
                   status = "danger",
                   fill = TRUE,
                   selected =  "Daily 7-Day Moving Avg"
                 ) 
                 
               ),
               mainPanel(
                 h2(textOutput("phase_text")),
                 plotOutput(outputId = "covid_cases"),
                 plotOutput(outputId = "distPlot_network")
               ),
             ),
                 
            #------------------------------- tab panel - 3 plots ---------------------------------
            tabPanel(
              "Covid-19 calls during the pandemic",
              icon=icon("phone"),
              # Main panel for displaying outputs ----
              mainPanel(
                # Output
                HTML("<h3><b>COVID-19 related complaints changed at different 
                <span style='color: DarkOrange'>time points</span> 
                of the pandemic and varied by 
                <span style='color: DarkOrange'>borough</span>
                </b></h3><br>
                     <h5>During the pandemic, 5 strictly COVID-19 related complaints were reported: 
                     <ol>
                      <li>Vaccine mandate non-compliance</li>
                      <li>Noncompliance with phased reopening</li>
                      <li>Mass gathering complaint</li>
                      <li>Face covering violation</li>
                      <li>Covid-19 non-essential construction</li>
                     </ol></h5><br>"
                ),
                plotOutput(outputId = "Barchart1"),
                HTML("<br><h3><b>Complaint types changed in response to 
                <span style='color: DarkOrange'>COVID-19 mandates. </span></b></h3><br>
                     <h5>In the early stages of the pandemic, NYC non-essential businesses and construction were mandated 
                     to shut down. Today, businesses are allowed to be open contingent on them following vaccine and face 
                     covering mandates.</h5><br>"
                ),
                plotOutput(outputId = "ts"),
                HTML("<br><h3><b>Each 
                     <span style='color: DarkOrange'>borough prioritized </span>
                     reporting different COVID-19 related violations.</b></h3><br>"
                ),
                selectInput('complaint_type',
                            label = 'Complaint Type',
                            choices = c("noncompliance with phased reopening",
                                        "covid-19 non-essential construction",
                                        "mass gathering complaint",
                                        "vaccine mandate non-compliance",
                                        "face covering violation"
                            ),
                            selected = "noncompliance with phased reopening"),
                plotOutput(outputId = "Barchart2"),
                HTML("<h5>Each borough's call volume was normalized by its population. The value plotted
                     is (call volume / population) / sum(across all boroughs) *100, so that all bars sum to 100. </h5><br>")
              )
            ),          
            #------------------------------- tab panel - Correlation ---------------------------------
            tabPanel(
              "Covid and non-Covid call relationship",
              icon=icon("phone"),
              # Main panel for displaying outputs ----
              mainPanel(
                # Output
                HTML("<h3><b>Some
                <span style='color: DarkOrange'>non-COVID complaints </span>
                were heavily correlated with COVID complaints</b></h3><br>
                     <h5>During the entirety of the pandemic, there were around 200 different complaint types, 
                     only 5 of which directly referenced COVID-19. However, many COVID and non-COVID calls were
                     correlated.</h5><br>"
                ),
                plotOutput(outputId = "Correlation"),
                HTML("<h5>It is good to note that many of the construction related complaints are clustered together 
                     (electric, plumbing, door/window, flooring/stairs, paint/plaster). These are all 
                     negatively correlated with COVID-19 non-essential construction.<br></h5>
                     
                     <h6> Figure details. Non-covid complaints shown are those with |corr| > 0.7 for at least one 
                     covid complaint. Face covering violations are not shown because there were too few data 
                     points for sound correlation analysis.</h6>"
                ),
                HTML("<br><h3><b>
                     <span style='color: DarkOrange'>Unfiltered </span>
                     correlation values</b></h3><br>"
                ),
                dataTableOutput(outputId = "Datatable"),
              )
            ),
            #------------------------------- tab panel - Conclusion ---------------------------------
            tabPanel(
              "References ",
              icon=icon("list"),
              HTML("<h3><b>Concluding Remarks</b> </h3>
                <h5>The pandemic did have an ubiquitous impact over the lives of New Yorkers and that was evident in the NYC311 call patterns from 2019 to 2022.</h5>
                <h5>The network diagrams indicated that residential and payment-related issues were the dominant call types pre-pandemic but transited to 
                <br>Covid-related calls at the onset of the pandemic. Throughout the pandemic, we saw an increase in calls pertaining to social distancing 
                <br>guidelines, symptoms and prevention, assistance in food delivery programs and livelihood, and vaccine-related matters. But as the pandemic 
                <br>waxed and waned the dominant issues that occurred pre-pandemic started to surface again.</h5>
                <h5>Also, the call complaint types changed over time in accordance with the Covid-19 health mandates. For example, during the early phases of 
                <br>the pandemic, businesses were ordered to shut down, and this resulted in high call volumes regarding non-compliance with phased opening, 
                <br>leading us to believe that most businesses were not in compliance with health mandates. During the later phases of the pandemic, however, 
                <br>the calls were mostly on vaccine and face covering violations, which is a consequence of recent vaccine and masking mandates.</h5>
                <h5>The correlation between Covid-19 and non-Covid-19 calls were interesting to note as well. For instance, many construction-related
                <br>complaints were clustered together and were negatively correlated with Covid-19 non-essential construction because residents could not do 
                <br>anything about it because non-essential work were not permitted (the high number of complaints were also corroborated by the network diagrams).</h5>"
              ),
              HTML("<h3><b>Data Sources</b> </h3>
              <h5> Both 311 datasets were downloaded one year at a time starting from 2019-present.</h5>
                <h5> <p><a href='https://data.beta.nyc/dataset/pediacities-nyc-neighborhoods/resource/7caac650-d082-4aea-9f9b-3681d568e8a5'>
                  NYC zip and population data</a></p></h5>
                <h5><p><a href='https://data.cityofnewyork.us/City-Government/311-Call-Center-Inquiry/wewp-mm3p' target='_blank'>
                   NYC 311 call center inquiry data</a></p></h5>
                <h5><p><a href='https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9' target='_blank'>
                   NYC 311 service requests data</a></p></h5>
                <h5><p><a href='https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9' target='_blank'>
                   NYC COVID case and death data</a></p></h5>"
              ),
              HTML("<h3><b>Contributors</b> </h3>
                <h5> <p>Jiazheng Chen (jc5656@columbia.edu)</p></h5>
                <h5><p>Christie Du (cd3250@columbia.edu)</p></h5>
                <h5><p>Marcus Loke (ml4636@columbia.edu)</p></h5>"
              ),
              HTML("<h3><b>Github Repository</b> </h3>
                <h5><p><a href='https://github.com/nytimes/covid-19-data' target='_blank'>
                   311 App Repository</a></p></h5>"
              ),
            ),
             
  ) #navbarPage closing  
) #Shiny UI closing    
