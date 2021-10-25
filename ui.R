navbarPage('VIC road accidents', theme = shinytheme("superhero"),
           
           #===================== Intro tab =====================
           tabPanel('Introduction',
                    div(
                      class='coverDiv',
                      tags$head(includeCSS('styles.css')), # custom styles
                      
                      absolutePanel(fixed = TRUE, draggable = FALSE, 
                                    top = 60, left = 50, right = 'auto', bottom = 'auto',
                                    width = 550, height = 'auto', 
                                    
                                    div( class = 'coverTextDiv',
                                         
                                         # Add some backgroup to introduction page
                                         h1('Victoria Road Accident Data Visualization'),
                                         h3('Everybody loves Victoria.'),
                                         h3('There are still many traffic accidents happened every day.'),
                                         h3('Preventing and reducing road traffic accidents is an inevitable 
                                            requirement for building a harmonious society and the basis for maintaining 
                                            sustained rapid, coordinated, and healthy economic development..'),
                                         h3('This Visualisation Project includes an interactive shiny app and some preliminary analysis result.'),
                                         br()
                                        )
                                    )
                    )
           ),
           #===================== Map View tab =====================
           tabPanel('Map View', inputId = 'mapViewTab',
                    div(class='outer',
                        tags$head(includeCSS('styles.css') # custom styles
                        ),
                        leafletOutput('myMap', width = '100%', height = '100%')
                    ),
                    
                    # Panel options: 
                    absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE, draggable = TRUE, 
                                  top = 40, left = 40, right = 'auto', bottom = 'auto',
                                  width = 320, height = 'auto',
                                  
                                  #h3('Pick data & Set map'), 
                                  
                                  fluidRow(
                                    column(6,
                                           selectInput(inputId = 'mapYear',
                                                       label = h4('Year'),
                                                       choices = YearAll,
                                                       selected = 'All')),
                                    column(6,
                                           
                                           selectInput(inputId = 'mapName',
                                                       label = h4('LGA_name'),
                                                       choices = LGAAll,
                                                       selected = 'All'))
                                  ),
                                  
                                  sliderInput(inputId = 'mapSliderMonth', label = h4('Month range'),
                                              min = 1, max = 12, value = c(1, 12), step = 1, 
                                              animate = animationOptions(interval = 1000, loop = T)), 
                                  fluidRow(
                                    column(6,
                                           checkboxGroupInput(inputId = 'mapSeverity', label = h4('Severity level'), 
                                                              choices = Sevr, 
                                                              selected = Sevr)),
                                    column(6,
                                           checkboxGroupInput(inputId = 'mapSpeed', label = h4('Speed zone'), 
                                                              choices = Speed, selected = Speed))
                                  ),
                                  
                                  h3('Set map'),
                                  
                                  fluidRow(
                                    column(6,
                                           checkboxInput('showHeatMap', label = h4('Heat map'), value = FALSE)),
                                    
                                    column(6,
                                           checkboxInput('showClusterMap', label = h4('Cluster map'), value = FALSE)))
                    )
                    
           ),
           #===================== Severity & Speed limit tab =====================
           tabPanel('Severity & Victims', 
                    
                    fluidRow(
                      column(2,
                             h3('Severity data'),
                             selectInput(inputId = 'plotSeverityYear',
                                         label = h4('Year'),
                                         choices = YearAll,
                                         selected = 'All'),
                             
                             
                             selectInput(inputId = 'plotSeverityLGA',
                                         label = h4('Area'),
                                         choices = LGAAll,
                                         selected = 'All')
                      ),
                      column(6,
                             plotlyOutput(outputId = 'barPlotYearSeverity')
                      ),
                      column(4,
                             plotlyOutput(outputId = 'PlotSeverity')
                      )
                    ),
                    br(),
                    fluidRow(
                      column(2,
                             h3('Victim data'),
                             selectInput(inputId = 'plotSpeedYear',
                                         label = h4('Year'),
                                         choices = YearAll,
                                         selected = 'All'),
                             
                             
                             selectInput(inputId = 'plotSpeedLGA',
                                         label = h4('Area'),
                                         choices = LGAAll,
                                         selected = 'All')
                      ),
                      column(6,
                             plotlyOutput(outputId = 'barPlotYearSpeed')
                      ),
                      column(4,
                             plotlyOutput(outputId = 'piePlotSpeed')
                      )
                      
                    ),
                    br(),
                    br()
           ),
           #===================== Factors tab =====================
           tabPanel('Factors', 
                    # If change background color, use:
                    # style = "background-color: #F4F4F4;", 
                    # this color code is for 'gray96', a comfortable background color.
                    wellPanel(
                      fluidRow(
                        column(6,
                               plotlyOutput(outputId = 'barPlotYearRegion', width = "100%")
                        ),
                        column(6,
                               plotlyOutput(outputId = 'freqpolyPlotMonth')
                        )
                      ),
                      br(),
                      fluidRow(
                        column(6,
                               plotlyOutput(outputId = 'freqpolyPlotSpeed_Zone')
                        ),
                        column(6,
                               plotlyOutput(outputId = 'freqpolyPlotHour')
                        )
                      ),
                      br()
                    ),
                    br()
           )
)