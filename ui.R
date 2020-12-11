library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)

shinyUI(dashboardPage(
  skin="purple",
  dashboardHeader(
    title = "Time Series Decomposition", 
    titleWidth = 266.2,
    tags$li(class = "dropdown", actionLink("info", icon("info"))),
    tags$li(class = "dropdown",
            tags$a(href='https://shinyapps.science.psu.edu/',
                   icon("home")))
    ),
  dashboardSidebar(
    width=250,
    sidebarMenu(
      id="tabs",
      menuItem("Overview", tabName = "intro", icon = icon("dashboard")),
      menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
      menuItem("Explore Plots", tabName = "plots", icon = icon("wpexplorer")),
      menuItem("Simulate plots", tabName = "modify", icon = icon("edit")),
      menuItem("Challenge", tabName = "challenge", icon = icon("cogs")),
      menuItem("References", tabName = "References", icon = icon("leanpub"))
      
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
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
              fluidPage(
                
                h1("Time series Decomposition"),
                p(
                  "This app is designed to decompose different time series and simulate time series under different seasonality, slope of long term trend"
                  
                ),
                p("Time Series Decomposition decompose time series to three parts and they are trend part, seasonal part and random part.Trend part refers to the general trend of the time series. The seasonal part refers to the regular(repetitive) part of time series and the random part is the remaining part that excludes seasonal part and trend part."),
                br(),
                h2("Instructions"),
                tags$ol(
                  tags$li(
                    "You can explore time series plots for several different datasets from real life.."
                  ),
                  
                  
                  tags$li(
                    "Check the 'decompose' box to look at seasonality, long term trend as well as random components separately."
                  ),
                  
                  
                  tags$li(
                    "Then you can hit 'NEXT' to go to the simulate plots part."
                  ),
                  
                  
                  tags$li("You can modify trend, seasonality and random components and see how the time series plot changes."),
                  
                  
                  tags$li(
                    "Then you can hit 'NEXT' to go to the challenge part."
                  ),
                  tags$li(
                    "In the challenge part, try to match the decompositions to the observed time series plot."
                  ), 
                  tags$li(
                    "Play with it and have fun!"
                  ),
                ),
                div(style = "text-align: center",
                    bsButton("go", "Go!", icon("book"), class =
                               "circle grow")),
                br(),
                h2("Acknowledgements:"),
                p(
                  "This app was developed and coded by Jiajun Gao in 2019 and modified by Jiawei Wu in 2020"
                ),
                
                div(class = "updated", "Last Update: 07/05/2020 by WJW.")
                
              )
      ),
      tabItem(
        tabName = "Prerequisites",
        h2("Prerequisites"),
        #https://online.stat.psu.edu/stat100/lesson/3/3.2
        #"https://online.stat.psu.edu/stat200/lesson/2/2.2/2.2.3"
        p(
          "In order to get the most out of this app, please review the
            following information that will be used in the app."
        ),
        tags$ul(
          tags$li("This app decompose the time series of Ford stock price, Berkshire Hathaway stock price, S&P500, State College weather, GDP growth rate of U.S.,U.S. covid-19 cases,Working hours, US employment rate and Exports of goods&services "),
          tags$li("The working hour in time series decompostion part refers to is the average weekly hours of production and nonsupervisory employees in manufacturing industry from year 1939 to 2020."),
          tags$li("Berkshire Hathaway is an American multinational conglomerate holding company headquartered in Omaha, Nebraska, United States. ")
        )
       
      ),
      #third tab content
      tabItem(tabName = "plots",
              tags$head(tags$style(
                HTML("input[type=\"number\"] {width: 60px;}")
              )),
              h2("explore plots"),
              p("In this section, you can explore the time series plots of differnt datasets and you can also see the decompostion of each time series plot. By Clicking the menu and you can select different datasets.By clicking the decompose, you can see the decompostion of the time series of the dataset."),

              fluidRow(
                column(4,
                       wellPanel(
                         style = "background-color: #EAF2F8",
                         selectInput("dataset",label=h3("Dataset"),
                                     choices = list("Ford Stock Price", "Berkshire Hathaway Stock Price", 
                                                    "S&P 500", "State College Weather", 
                                                    "GDP growth rate of U.S.","U.S. covid-19 cases","Working hours","US employment rate","Exports of goods&services"),
                                     selected = "Ford Stock Price"
                         ),
                         br(),
                         checkboxInput(inputId="decompose", label = list("decompose"), value = TRUE),
               
               
  

              
                         
                         
                       )),
                fluidRow(
                  column(7,
                         wellPanel(
                           style = "background-color: #EAF2F8",
                           plotOutput("timeseriesplot", height = 300),
                 
                         )
                  )
                ),
                column(4,
                       wellPanel(style = "background-color: #EAF2F8",
                                 conditionalPanel(condition="input.dataset=='Ford Stock Price'",
                                                  br(),
                                                  h6("information:"),
                                                  h6("This dataset is monthly stock price (close price) of Ford from July 1988 to June 2017.")
                                 ),
                                 conditionalPanel(condition="input.dataset=='Berkshire Hathaway Stock Price'",
                                                  br(),
                                                  h6("information:"),
                                                  h6("This dataset is monthly stock price (close price) of Berkshire Hathaway from July, 1988 to June 2017. ")
                                 ),
                                 conditionalPanel(condition="input.dataset=='S&P 500'",
                                                  br(),
                                                  h6("information:"),
                                                  h6("This dataset is S&P 500 index from January, 1988 to June 2017.")
                                 ),
                                 conditionalPanel(condition="input.dataset=='State College Weather'",
                                                  br(),
                                                  h6("information:"),
                                                  h6("This dataset is monthly mean temperature of State College from January, 1988 to December 2017.")
                                 ),    
                                 conditionalPanel(condition="input.dataset=='GDP growth rate of U.S.'",
                                                         br(),
                                                         h6("information:"),
                                                         h6("This dataset is quarterly GDP growth rate of U.S. from January, 1988 to January 2018.")
                                 ),
                                 conditionalPanel(condition="input.dataset=='U.S. covid-19 cases'",
                                                  br(),
                                                  h6("information:"),
                                                  h6("This dataset is the covid-19 cases in us on daily basis from January 21,2020 to June 13,2020.")
                                 ),
                                 conditionalPanel(condition="input.dataset=='Working hours'",
                                                  br(),
                                                  h6("information:"),
                                                  h6("This dataset is the average weekly hours of productin and nonsupervisory employees in manufacrtueing industry from year 1939 to 2020.")
                                 ),
                                 conditionalPanel(condition="input.dataset=='US employment rate'",
                                                  br(),
                                                  h6("information:"),
                                                  h6("This dataset is US employment rate of age 15-64 people from 1977 to 2020.")
                                                  
                                 ),
                                 conditionalPanel(condition="input.dataset=='Exports of goods&services'",
                                                  br(),
                                                  h6("information:"),
                                                  h6("This data is exports of goods and services of the U.S. from year 1992 to 2020")
                                                  
                                 ),
                                 
                                 
                                 )),
              
                
                
                column(7, 
                       wellPanel(
                         style = "background-color: #EAF2F8",
                         plotOutput("decomposeplot", height = 300)
                         # if (input$decompose == "TRUE") {bsPopover("decomposeplot", " ", 
                         #           "This shows the original plot, seasonality, long term trend and remainder of the dataset", 
                         #           place = "left", trigger = "hover")
                         # }
                         #   plotOutput("decompose1", height = 250),
                         #   plotOutput("decompose2", height = 250),
                         #   plotOutput("decompose3", height = 250)
                       )
                ),
                
                column(4,
                       
                       br(),
                       br(),
                       style = "text-align: center" , bsButton("nextpart", "NEXT", size = "large", style = "warning", center = TRUE)),
                bsPopover("nextpart", " ", 
                          "Go to simulate plots.", 
                          place = "right", trigger = "hover")
                
                
                
              )
              
              
      ),
      
      #third tab content
      tabItem(tabName = "modify",
              tags$head(tags$style(
                HTML("input[type=\"number\"] {width: 60px;}")
              )),
              h2("Simulate plots"),
              p("In this section, you can simulate the time series plot and you can change the slope of long term trend, seasonality and random error which is three parts to form the time series. You can also click the menu to choose how many process you want to show. "),
              
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
              tags$head(tags$style(
                HTML("input[type=\"number\"] {width: 60px;}")
              )),
              h2("Challenge"),
              p("In this section, you can answer the question and choose the right answer from A, B or C"),
              

              verbatimTextOutput("question1"),
              
              wellPanel(style = "background-color: #EAF2F8",
                        fluidRow(
                          column(5,
                                 imageOutput("questiongraph", height = 280),
                                 tags$head(tags$style(HTML("#question1 {font-size: 22px;}"))),
                                 tags$style(type='text/css', '#question1 {background-color: #ffffff; color: black;}') 
                          ),
                          column(3, offset = 3,
                                 br(),
                                 br(),
                                 actionButton("newchallenge","New Challenge"),
                                 br(),
                                 br(),
                                 br(),
                                 selectInput("answer", "Select your answer", choices = list("A", "B", "C", ""), selected = ""),
                                
                            
                          )
                          
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(4, offset = 6,
                                 verbatimTextOutput("response"),
                               
                                
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
              
      ),
      #### Set up the References Page
      tabItem(
        tabName = "References",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R
          package version 1.4.0.2. https://CRAN.R-project.org/package=shiny"
        ),
        p(
          class = "hangingindent",
          " Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: Create Dashboards with 'Shiny'. R package version 0.7.1.
  https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          " Dean Attali (2020). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 1.1.
  https://CRAN.R-project.org/package=shinyjs"
        ),
        p(
          class = "hangingindent",
          "Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61.
  https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          "Andrew K. Smith (2019). CombMSC: Combined Model Selection Criteria. R package version 1.4.2.1.
  https://CRAN.R-project.org/package=CombMSC"
        ),
        p(
          class="hangingindent",
          "U.S. Bureau of Economic Analysis and U.S. Census Bureau, Exports of Goods and Services, Balance of Payments Basis [BOPTEXP], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/BOPTEXP, June 18, 2020."
        ),
        p(
          class= "hangingindent",
          "U.S. Bureau of Labor Statistics, Average Weekly Hours of Production and Nonsupervisory Employees, Manufacturing [AWHMAN], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/AWHMAN, June 18, 2020."
        ),
        p(
          class = "hangingindent",
          "Organization for Economic Co-operation and Development, Employment Rate: Aged 15-64: All Persons for the United States [LREM64TTUSM156S], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/LREM64TTUSM156S, June 18, 2020."
        )
        ,
        
        p(
          class="hangingindent",
          "Nytimes. (2020, June 19). Nytimes/covid-19-data. Retrieved June 19, 2020, from https://github.com/nytimes/covid-19-data"
        ),
        p(class="hangingindent",
          "S&amp;P 500 (^GSPC) Charts, Data &amp; News. (2020, June 19). Retrieved June 19, 2020, from https://finance.yahoo.com/quote/^GSPC?p=%5EGSPC
"),
        p(
          class="hangingindent",
          "Berkshire Hathaway Inc. (BRK-A) Stock Price, Quote, History &amp; News. (2020, June 19). Retrieved June 19, 2020, from https://finance.yahoo.com/quote/BRK-A?p=BRK-A"
        ),
        p(
          class="hangingindent",
          "Forward Industries, Inc. (FORD) Stock Price, Quote, History &amp; News. (2020, June 19). Retrieved June 19, 2020, from https://finance.yahoo.com/quote/FORD?p=FORD
"
        ),
        p(
          class="hangingindent",
          "Federal Reserve Economic Data: FRED: St. Louis Fed. (n.d.). Retrieved June 19, 2020, from https://fred.stlouisfed.org/
"
        ),
        p(
          class="hangingindent",
          "US Department of Commerce, N. (n.d.). National Weather Service. Retrieved June 19, 2020, from https://w2.weather.gov/
"
        ),
        p(
          class="hangingindent",
          "“Berkshire Hathaway.” Wikipedia, Wikimedia Foundation, 2 July 2020, en.wikipedia.org/wiki/Berkshire_Hathaway.
"
        ),
        
      )
      
    )
  )
  
)

)
