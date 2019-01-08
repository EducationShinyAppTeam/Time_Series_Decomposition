library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)

shinyUI(dashboardPage(#skin="yellow",
  
  dashboardHeader(title = "Time Series Decomposition", titleWidth = 250),
  dashboardSidebar(
    sidebarMenu(
      id="tabs",
      menuItem("Overview", tabName = "intro", icon = icon("dashboard")),
      menuItem("Explore Plots", tabName = "plots", icon = icon("wpexplorer")),
      menuItem("Simulate plots", tabName = "modify", icon = icon("edit")),
      menuItem("Challenge", tabName = "challenge", icon = icon("cogs"))
      
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
    ),
    # tags$head( 
    #   tags$style(HTML(
    #     '.popover-title{
    #     color: black;
    #     background-color: orange }'
    #   ))),
    tabItems(
      
      # First tab content
      tabItem(tabName = "intro",
              tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
              br(),br(),br(),
              h3(tags$b("About:")),
              h4("This application is to help you better understand seasonality, long term trend and random components of time series plot."),
              br(),
              h3(strong("Instructions:")),
              h4(tags$li("You can explore time series plots for several different datasets from real life.")),
              h4(tags$li("Check the 'decompose' box to look at seasonality, long term trend as well as random components separately.")),
              h4(tags$li("You can look at the information of each dataset by checking 'information' box.")),
              h4(tags$li("Then you can hit 'NEXT' to go to the simulate plots part.")),
              h4(tags$li("You can modify trend, seasonality and random components and see how the time series plot changes.")),
              h4(tags$li("Then you can hit 'NEXT' to go to the challenge part.")),
              h4(tags$li("In the challenge part, try to match the decompositions to the observed time series plot.")),
              h4(tags$li("Play with it and have fun!")),
              br(),
              div(style = "text-align: center" ,bsButton("start", "Explore", size = "large", style = "warning", icon = icon("bolt"))),
              h3(strong("Acknowledgements:")),
              h4("This app was developed and coded by Jiajun Gao."),
              h4("Built-in datasets are downloaded from finance.yahoo.com, w2.weather.gov, and fred.stlouisfed.org")
      ),
      
      #Second tab content
      tabItem(tabName = "plots",
              div(style="display: inline-block;vertical-align:top;",
                  tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
              ),
              fluidRow(
                column(4,
                       wellPanel(
                         style = "background-color: #EAF2F8",
                         selectInput("dataset",label=h3("Dataset"),
                                     choices = list("Ford Stock Price", "Berkshire Hathaway Stock Price", 
                                                    "S&P 500", "State College Weather", 
                                                    "GDP growth rate of U.S." ),
                                     selected = "Ford Stock Price"
                         ),
                         br(),
                         checkboxInput(inputId="decompose", label = list("decompose"), value = TRUE),
                         br(),
                         checkboxInput(inputId = "info", label = "information")
                         
                         
                       )),
                fluidRow(
                  column(7,
                         wellPanel(
                           style = "background-color: #EAF2F8",
                           plotOutput("timeseriesplot", height = 300),
                           bsPopover("timeseriesplot", " ", 
                                     "This is time series plot of the chosen dataset", 
                                     place = "right", trigger = "hover")
                         )
                  )
                ),
                
                
                
                column(4,
                       wellPanel(
                         style = "background-color: #EAF2F8",
                         textOutput("information")
                       ),
                       br(),
                       br(),
                       style = "text-align: center" , bsButton("nextpart", "NEXT", size = "large", style = "warning", center = TRUE)),
                bsPopover("nextpart", " ", 
                          "Go to simulate plots.", 
                          place = "right", trigger = "hover"),
                
                
                
                column(7, 
                       wellPanel(
                         style = "background-color: #EAF2F8",
                         plotOutput("decomposeplot", height = 600)
                         # if (input$decompose == "TRUE") {bsPopover("decomposeplot", " ", 
                         #           "This shows the original plot, seasonality, long term trend and remainder of the dataset", 
                         #           place = "left", trigger = "hover")
                         # }
                         #   plotOutput("decompose1", height = 250),
                         #   plotOutput("decompose2", height = 250),
                         #   plotOutput("decompose3", height = 250)
                       )
                )
                
                
              )
              
              
      ),
      
      #third tab content
      tabItem(tabName = "modify",
              div(style="display: inline-block;vertical-align:top;",
                  tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
              ),
              # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange}")),
              # tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: orange}")),
              # tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: orange}")),
              # tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: orange}")),
              # 
              fluidRow(
                column(4,
                       wellPanel(
                         style = "background-color: #EAF2F8",
                         sliderInput(inputId = "trend", label = "slope of long term trend", min = -15, max = 15, value = 0),
                         sliderInput(inputId = "season", label = "seasonality (amplitude)", min = 0, max = 200, value = 0),
                         sliderInput(inputId = "random", label = "random error (s.d. of the error)", min = 0, max = 200, value = 0),
                         #sliderInput(inputId = "freq", label = "seasonality (frequency)", min = 1, max = 12, value = 1),
                         
                         #####try mulyiple
                         selectInput("simulation",label=h3("Simulation"),
                                     choices = list("single process", "multiple processes"),
                                     selected = "single process")
                         ####end of wellPanel
                       ),
                       
                       conditionalPanel(condition = "input.simulation == 'single process'",
                                        style = "text-align: center" , bsButton("newpro", "New Process", size = "middle", style = "warning")),
                       
                       ####add a conditonal slide
                       conditionalPanel(condition = "input.simulation == 'multiple processes'",
                                        sliderInput("path", "# of paths", min = 1, max = 3, value = 1))
                       
                ),
                
                
                
                column(8,
                       wellPanel(
                         style = "background-color: #EAF2F8",
                         plotOutput("simplot", height = 500)
                       )
                )
                
                
              ),
              
              br(),
              
              fluidRow(
                div(style = "text-align: center" ,bsButton("next2", "NEXT", size = "large", style = "warning")),
                bsPopover("next2", " ", 
                          "Go to the challenge part", 
                          place = "right", trigger = "hover")
              )
      ),
      
      
      
      #fourth tab content
      tabItem(tabName = "challenge",
              div(style="display: inline-block;vertical-align:top;",
                  tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
              ),
              verbatimTextOutput("question1"),
              
              wellPanel(style = "background-color: #EAF2F8",
                        fluidRow(
                          column(5,
                                 imageOutput("questiongraph", height = 280),
                                 tags$head(tags$style(HTML("#question1 {font-size: 22px;}"))),
                                 tags$style(type='text/css', '#question1 {background-color: #ffffff; color: black;}') 
                          ),
                          column(2, offset = 2,
                                 br(),
                                 br(),
                                 actionButton("newchallenge","New Challenge"),
                                 br(),
                                 br(),
                                 br(),
                                 selectInput("answer", "Select your answer", choices = list("A", "B", "C", ""), selected = "")
                          )
                          
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(4, offset = 6,
                                 verbatimTextOutput("response")
                          ))
                        
                        
              ),
              
              wellPanel(style = "background-color: #EAF2F8",
                        fluidRow(
                          column(4,
                                 imageOutput("choice1", height = 250)),
                          column(4,
                                 imageOutput("choice2", height = 250)),
                          column(4,
                                 imageOutput("choice3", height = 250))
                        )     
                        
              )
              
      )
      
    )
  )
  
)

)
