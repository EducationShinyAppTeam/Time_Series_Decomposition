# Load Package ----
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)

library(boastUtils)

# Define UI for App ----

ui <- list(
  dashboardPage(
    skin="yellow",
    ## Header ----
    dashboardHeader(
      title = "Time Series Decomposition", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Time_Series_Decomposition")
        ),
      tags$li(
        class = "drowdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home"))
        )
      ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id="tabs",
        menuItem("Overview", tabName = "intro", icon = icon("dashboard")),
        menuItem("Explore Plots", tabName = "plots", icon = icon("wpexplorer")),
        menuItem("Simulate plots", tabName = "modify", icon = icon("edit")),
        menuItem("Challenge", tabName = "challenge", icon = icon("cogs"))
        ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
      ),
      tabItems(
        # First tab content
        ### intro ----
        tabItem(
          tabName = "intro",
                h3(tags$b("About:")),
                h4("This application is to help you better understand 
                   seasonality, long term trend and random components of time 
                   series plot."),
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
        # Second tab content
        ### plots page ----
        tabItem(
          tabName = "plots",
            fluidRow(
              column(4,
                wellPanel(
                style = "background-color: #EAF2F8",
                selectInput(
                  "dataset",
                  label=h3("Dataset"),
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
                         )
                  )
                )
        ),
        
        #third tab content
        ### Simulate polts ----
        tabItem(
          tabName = "modify",
            fluidRow(
              column(4,
                wellPanel(
                  style = "background-color: #EAF2F8",
                  sliderInput(inputId = "trend", label = "slope of long term trend", min = -15, max = 15, value = 0),
                  sliderInput(inputId = "season", label = "seasonality (amplitude)", min = 0, max = 200, value = 0),
                  sliderInput(inputId = "random", label = "random error (s.d. of the error)", min = 0, max = 200, value = 0),
                  selectInput(
                    "simulation",
                    label=h3("Simulation"),
                    choices = list("single process", "multiple processes"),
                    selected = "single process")
                    ),
                  conditionalPanel(
                    condition = "input.simulation == 'single process'",
                    style = "text-align: center" , 
                    bsButton(
                      "newpro", "New Process", 
                      size = "middle", 
                      style = "warning")
                    ),
                    ####add a conditonal slide
                    conditionalPanel(
                      condition = "input.simulation == 'multiple processes'",
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
        #### Challenge ----
        tabItem(tabName = "challenge",
                verbatimTextOutput("question1"),
                
                wellPanel(
                  style = "background-color: #EAF2F8",
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


server <- function(input, output, session) {
  
  observeEvent(input$start, {
    updateTabItems(session, "tabs", "plots")
  })
  
  observe({
    if (input$dataset == "Ford Stock Price"){
      ford <- read.csv(file = "ford.csv")
      time = ford[[1]]
      price = ford[[5]]
      #smooth_ford = filter(price, filter = c(1/24, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/24), sides = 2)
      ts_ford = ts(price, frequency = 12, start = c(1988,7), end = c(2018,6))
      #smooth_ford = ts(smooth_ford, frequency = 12, start = c(1988,7), end = c(2018,6))
      output$timeseriesplot <- renderPlot(plot.ts(ts_ford, ylab = "price"))
      
      if (input$decompose == "TRUE"){
        
        #ford2 = decompose(smooth_ford)
        ford2 = decompose(ts_ford)
        output$decomposeplot <-renderPlot(plot(ford2))
        
      }
      
      else if (input$decompose == "FALSE"){
        output$decomposeplot = NULL
      }
      
      if (input$info == "TRUE"){
        output$information <- renderText("This dataset is monthly stock price (close price) of Ford from July 1988 to June 2017.
                                         Data retrieved from finance.yahoo.com")
      }
      else if (input$info == "FALSE"){
        output$information = NULL
      }
    }
    
    else if (input$dataset == "Berkshire Hathaway Stock Price"){
      bh <- read.csv(file = "BRK-A.csv")
      time = bh[[1]]
      price = bh[[5]]
      #output$timeseriesplot <- renderPlot(plot.ts(price))
      ts_bh = ts(price, frequency = 12, start = c(1988,7), end = c(2018,6))
      output$timeseriesplot <- renderPlot(plot.ts(ts_bh, ylab = "price"))
      
      if (input$decompose == "TRUE"){
        bh2 = decompose(ts_bh, filter = c(1/24, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/24))
        output$decomposeplot <-renderPlot(plot(bh2))
      }
      
      else if (input$decompose == "FALSE"){
        output$decompose = NULL
      }
      
      if (input$info == "TRUE"){
        output$information <- renderText("This dataset is monthly stock price (close price) of Berkshire Hathaway from July, 1988 to June 2017.
                                         Data retrieved from finance.yahoo.com")
      }
      else if (input$info == "FALSE"){
        output$information = NULL
      }
    }
    
    else if (input$dataset == "S&P 500"){
      bh <- read.csv(file = "sp500.csv")
      time = bh[[1]]
      price = bh[[5]]
      ts_sp = ts(price, frequency = 12, start = c(1988,1), end = c(2018,6))
      output$timeseriesplot <- renderPlot(plot.ts(ts_sp, ylab = " "))
      
      if (input$decompose == "TRUE"){
        sp2 = decompose(ts_sp, filter = c(1/24, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/24))
        output$decomposeplot <-renderPlot(plot(sp2))
      }
      
      else if (input$decompose == "FALSE"){
        output$decompose = NULL
      }
      
      if (input$info == "TRUE"){
        output$information <- renderText("This dataset is S&P 500 index from January, 1988 to June 2017.
                                         Data retrieved from finance.yahoo.com")
      }
      else if (input$info == "FALSE"){
        output$information = NULL
      }
    }
    
    else if (input$dataset == "State College Weather"){
      sc <- read.csv(file = "StateCollegeWeather.csv")
      time = sc[[1]]
      temp = sc[[2]]
      ts_sc = ts(temp, frequency = 12, start = c(1988,1), end = c(2017,12))
      output$timeseriesplot <- renderPlot(plot.ts(ts_sc, ylab = "temperature"))
      
      if (input$decompose == "TRUE"){
        
        sc2 = decompose(ts_sc, filter = c(1/24, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12, 1/24))
        output$decomposeplot <-renderPlot(plot(sc2))
      }
      
      else if (input$decompose == "FALSE"){
        output$decomposeplot = NULL
      }
      if (input$info == "TRUE"){
        output$information <- renderText("This dataset is monthly mean temperature of State College from January, 1988 to December 2017.
                                         Data retrieved from w2.weather.gov")
      }
      else if (input$info == "FALSE"){
        output$information = NULL
      }
    }
    
    else if (input$dataset == "GDP growth rate of U.S."){
      sc <- read.csv(file = "gdp.csv")
      time = sc[[1]]
      gdp = sc[[2]]
      ts_gdp = ts(gdp, frequency = 4, start = c(1988,1), end = c(2018,1))
      output$timeseriesplot <- renderPlot(plot.ts(ts_gdp, ylab = "percent change"))
      
      if (input$decompose == "TRUE"){
        
        gdp2 = decompose(ts_gdp, filter = c(1/8, 1/4, 1/4, 1/4, 1/8))
        output$decomposeplot <-renderPlot(plot(gdp2))
      }
      
      else if (input$decompose == "FALSE"){
        output$decomposeplot = NULL
      }
      if (input$info == "TRUE"){
        output$information <- renderText("This dataset is quarterly GDP growth rate of U.S. from January, 1988 to January 2018.
                                         Data retrieved from fred.stlouisfed.org")
      }
      else if (input$info == "FALSE"){
        output$information = NULL
      }
    }
    
    
    
    
  })
  
  observeEvent(input$nextpart, {
    updateTabItems(session, "tabs", "modify")
  })
  
  
  
  
  observeEvent(
    input$simulation,{
      
      if (input$simulation == "single process"){
        updateSliderInput(session, "random", value = 0)
        updateSliderInput(session, "trend", value = 0)
        updateSliderInput(session, "season", value = 0)
        
        y.sim2 <- eventReactive(
          {input$random
            input$trend
            input$season}, {
              x <- input$random
              t = c(1: 50)
              
              f <- input$season
              temp = 45
              
              error.model = function(x){rnorm(n = 50, sd=x, mean=0)}
              
              set.seed(temp)
              
              y.sim = arima.sim(n = 50, list(ar = c(0.5), ma = c(0.5)), rand.gen = error.model)
              if (input$season == 0 & input$random == 0){
                y.sim2 = input$trend * t}
              else{
                y.sim2 = input$trend * t + y.sim + rnorm(n = 50, sd=x, mean=0) + f*cos(12*t)}
              
              
              
            }
        )
        output$simplot <- renderPlot(plot.ts(y.sim2(), ylab = "value", col = rgb(191, 116, 224, maxColorValue = 255), lwd = 2))
        
        observeEvent(input$newpro, {
          test <- sample(1:40, 1)
          
          updateSliderInput(session, "random", value = 0)
          updateSliderInput(session, "trend", value = 0)
          updateSliderInput(session, "season", value = 0)
          
          y.sim2 <- eventReactive(
            {input$random
              input$trend
              input$season}, {
                x <- input$random
                t = c(1: 50)
                
                f <- input$season
                set.seed(test)
                error.model = function(x){rnorm(n = 50, sd=x, mean=0)}
                y.sim = arima.sim(n = 50, list(ar = c(0.5), ma = c(0.5)), rand.gen = error.model)
                if (input$season == 0 & input$random == 0){
                  y.sim2 = input$trend * t}
                else{
                  y.sim2 = input$trend * t + y.sim + rnorm(n = 50, sd=x, mean=0) + f*cos(12*t)}
                
                
              }
          )
          output$simplot <- renderPlot(plot.ts(y.sim2(), ylab = "value", col = rgb(191, 116, 224, maxColorValue = 255), lwd = 2))
        })
        
        
      }
      
      
      
      else if (input$simulation == "multiple processes"){
        updateSliderInput(session, "random", value = 0)
        updateSliderInput(session, "trend", value = 0)
        updateSliderInput(session, "season", value = 0)
        
        a1 <- eventReactive({
          input$random
          input$trend
          input$season},{
            x <- input$random
            t = c(1: 50)
            f <- input$season
            error.model = function(x){rnorm(n = 50, sd=x, mean=0)}
            a = arima.sim(n = 50, list(ar = c(0.5), ma = c(0.5)), rand.gen = error.model)
            if (input$season == 0 & input$random == 0){
              a1 = input$trend * t}
            else{
              a1 = input$trend * t + a + rnorm(n = 50, sd=x, mean=0) + f*cos(12*t)}
            #a1 = input$trend * t + a + rnorm(n = 50, sd=x, mean=0) + f*cos(12*t)
          })
        b1 <- eventReactive({
          input$random
          input$trend
          input$season},{
            x <- input$random
            t = c(1: 50)
            f <- input$season
            error.model = function(x){rnorm(n = 50, sd=x, mean=0)}
            b = arima.sim(n = 50, list(ar = c(0.5), ma = c(0.5)), rand.gen = error.model)
            if (input$season == 0 & input$random == 0){
              b1 = input$trend * t}
            else{
              b1 = input$trend * t + b + rnorm(n = 50, sd=x, mean=0) + f*cos(12*t)}
            #b1 = input$trend * t + b + rnorm(n = 50, sd=x, mean=0) + f*cos(12*t)
          })
        c1 <- eventReactive({
          input$random
          input$trend
          input$season},{
            x <- input$random
            t = c(1: 50)
            f <- input$season
            error.model = function(x){rnorm(n = 50, sd=x, mean=0)}
            c = arima.sim(n = 50, list(ar = c(0.5), ma = c(0.5)), rand.gen = error.model)
            if (input$season == 0 & input$random == 0){
              c1 = input$trend * t}
            else{
              c1 = input$trend * t + c + rnorm(n = 50, sd=x, mean=0) + f*cos(12*t)}
            #c1 = input$trend * t + c + rnorm(n = 50, sd=x, mean=0) + f*cos(12*t)
            
          })
        
        
        
        observeEvent(
          {input$path
            input$season
            input$random}, {
              if (input$path == 1){
                output$simplot <- renderPlot(ts.plot(a1(), ylab = "value", lwd = 2, col = rgb(191, 116, 224, maxColorValue = 255)))
              }
              else if (input$path == 2){
                if (input$season == 0 & input$random == 0){
                  output$simplot <- renderPlot(ts.plot(b1(), ylab = "value", lwd = 2, col = rgb(99, 235, 235, maxColorValue = 255)))
                }
                else{
                  output$simplot <- renderPlot(ts.plot(a1(), b1(), ylab = "value", lwd = 2, gpars = list(col = c(rgb(191, 116, 224, maxColorValue = 255), rgb(99, 235, 235, maxColorValue = 255)))))
                }
              }
              else if (input$path == 3){
                if (input$season == 0 & input$random == 0){
                  output$simplot <- renderPlot(ts.plot(c1(), ylab = "value", lwd = 2, col = rgb(255, 105, 180, maxColorValue = 255)))
                }
                else{
                  output$simplot <- renderPlot(ts.plot(a1(), b1(), c1(), ylab = "value", lwd = 2, gpars = list(col = c(rgb(191, 116, 224, maxColorValue = 255), rgb(99, 235, 235, maxColorValue = 255), rgb(255, 105, 180, maxColorValue = 255)))))
                }
              }
            })
        
        
      }
    })
  
  
  
  
  observeEvent(input$next2, {
    updateTabItems(session, "tabs", "challenge")
  })
  
  
  #generate challenges
  c <- reactiveValues(right=c(sample(1:11,1)))
  
  observeEvent(input$newchallenge,{
    c$right=sample(1:11,1)
    updateSelectInput(session, inputId = "answer", selected = "")
  })
  
  output$question1<- renderText({
    if (c$right == 1){
      "Challenge: please choose the plot of long term trend of the following time series plot"
    }
    else if (c$right == 2){
      "Challenge: please choose the plot of seasonality of the following time series plot"
    }
    else if (c$right == 3){
      "Challenge: please choose the corresponding time series plot based on the following decomposed plots"
    }
    else if (c$right == 4){
      "Challenge: please choose the plot of long term trend of the following time series plot"
    }
    else if (c$right == 5){
      "Challenge: please choose the plot of seasonality of the following time series plot"
    }
    else if (c$right == 6){
      "Challenge: please choose the corresponding time series plot based on the following decomposed plots"
    }
    else if (c$right == 7){
      "Challenge: please choose the plot of long term trend of the following time series plot"
    }
    else if (c$right == 8){
      "Challenge: please choose the plot of long term trend of the following time series plot"
    }
    else if (c$right == 9){
      "Challenge: please choose the corresponding time series plot based on the following decomposed plots"
    }
    else if (c$right == 10){
      "Challenge: please choose the corresponding time series plot based on the following decomposed plots"
    }
    else if (c$right == 11){
      "Challenge: please choose the plot of seasonality of the following time series plot"
    }
  })
  
  output$questiongraph <- renderImage({
    if (c$right == 1){
      return(list(
        src = "q1.jpg"))
    }
    else if (c$right == 2){
      return(list(
        src = "q2.jpg"))
    }
    else if (c$right == 3){
      return(list(
        src = "q3.jpg"))
    }
    else if (c$right == 4){
      return(list(
        src = "q4.jpg"))
    }
    else if (c$right == 5){
      return(list(
        src = "q5.jpg"))
    }
    else if (c$right == 6){
      return(list(
        src = "q6.jpg"))
    }
    else if (c$right == 7){
      return(list(
        src = "q7.jpg"))
    }
    else if (c$right == 8){
      return(list(
        src = "q8.jpg"))
    }
    else if (c$right == 9){
      return(list(
        src = "q9.jpg"))
    }
    else if (c$right == 10){
      return(list(
        src = "q10.jpg"))
    }
    else if (c$right == 11){
      return(list(
        src = "q11.jpg"))
    }
  }, deleteFile = FALSE)
  
  output$choice1 <- renderImage({
    if (c$right == 1){
      return(list(
        src = "answer1a.jpg"))
    }
    else if (c$right == 2){
      return(list(
        src = "answer2a.jpg"))
    }
    else if (c$right == 3){
      return(list(
        src = "answer3a.jpg"))
    }
    else if (c$right == 4){
      return(list(
        src = "answer4a.jpg"))
    }
    else if (c$right == 5){
      return(list(
        src = "answer5a.jpg"))
    }
    else if (c$right == 6){
      return(list(
        src = "answer6a.jpg"))
    }
    else if (c$right == 7){
      return(list(
        src = "answer7a.jpg"))
    }
    else if (c$right == 8){
      return(list(
        src = "answer8a.jpg"))
    }
    else if (c$right == 9){
      return(list(
        src = "answer9a.jpg"))
    }
    else if (c$right == 10){
      return(list(
        src = "answer10a.jpg"))
    }
    else if (c$right == 11){
      return(list(
        src = "answer11a.jpg"))
    }
  }, deleteFile = FALSE)
  
  
  output$choice2 <- renderImage({
    if (c$right == 1){
      return(list(
        src = "answer1b.jpg"))
    }
    else if (c$right == 2){
      return(list(
        src = "answer2b.jpg"))
    }
    else if (c$right == 3){
      return(list(
        src = "answer3b.jpg"))
    }
    else if (c$right == 4){
      return(list(
        src = "answer4b.jpg"))
    }
    else if (c$right == 5){
      return(list(
        src = "answer5b.jpg"))
    }
    else if (c$right == 6){
      return(list(
        src = "answer6b.jpg"))
    }
    else if (c$right == 7){
      return(list(
        src = "answer7b.jpg"))
    }
    else if (c$right == 8){
      return(list(
        src = "answer8b.jpg"))
    }
    else if (c$right == 9){
      return(list(
        src = "answer9b.jpg"))
    }
    else if (c$right == 10){
      return(list(
        src = "answer10b.jpg"))
    }
    else if (c$right == 11){
      return(list(
        src = "answer11b.jpg"))
    }
  }, deleteFile = FALSE)
  
  output$choice3 <- renderImage({
    if (c$right == 1){
      return(list(
        src = "answer1c.jpg"))
    }
    else if (c$right == 2){
      return(list(
        src = "answer2c.jpg"))
    }
    else if (c$right == 3){
      return(list(
        src = "answer3c.jpg"))
    }
    else if (c$right == 4){
      return(list(
        src = "answer4c.jpg"))
    }
    else if (c$right == 5){
      return(list(
        src = "answer5c.jpg"))
    }
    else if (c$right == 6){
      return(list(
        src = "answer6c.jpg"))
    }
    else if (c$right == 7){
      return(list(
        src = "answer7c.jpg"))
    }
    else if (c$right == 8){
      return(list(
        src = "answer8c.jpg"))
    }
    else if (c$right == 9){
      return(list(
        src = "answer9c.jpg"))
    }
    else if (c$right == 10){
      return(list(
        src = "answer10c.jpg"))
    }
    else if (c$right == 11){
      return(list(
        src = "answer11c.jpg"))
    }
  }, deleteFile = FALSE)
  
  output$response <- renderText({
    if ((c$right == 1) & (input$answer == "C")){
      "Correct!"
    }   
    else if ((c$right == 1) & (input$answer == "B")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 1) & (input$answer == "A")){
      "Sorry, please try again.\nHint: Look at the scale"
    }
    else if ((c$right == 2) & (input$answer == "C")){
      "Correct!"
    }
    else if ((c$right == 2) & (input$answer == "A")){
      "Sorry, please try again."
    }
    else if ((c$right == 2) & (input$answer == "B")){
      "Sorry, please try again."
    }
    else if ((c$right == 3) & (input$answer == "A")){
      "Correct!"
    }   
    else if ((c$right == 3) & (input$answer == "B")){
      "Sorry, please try again.\nHint: please double check the seasonality."
    }
    else if ((c$right == 3) & (input$answer == "C")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 4) & (input$answer == "B")){
      "Correct!"
    }
    else if ((c$right == 4) & (input$answer == "A")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 4) & (input$answer == "C")){
      "Sorry, please try again.\nHint: Look at the scale"
    }
    else if ((c$right == 5) & (input$answer == "A")){
      "Correct!"
    }
    else if ((c$right == 5) & (input$answer == "B")){
      "Sorry, please try again."
    }
    else if ((c$right == 5) & (input$answer == "C")){
      "Sorry, please try again."
    }
    else if ((c$right == 6) & (input$answer == "B")){
      "Correct!"
    }
    else if ((c$right == 6) & (input$answer == "A")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 6) & (input$answer == "C")){
      "Sorry, please try again.\nHint: please double check the seasonality."
    }
    else if ((c$right == 7) & (input$answer == "C")){
      "Correct!"
    }
    else if ((c$right == 7) & (input$answer == "A")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 7) & (input$answer == "B")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 8) & (input$answer == "B")){
      "Correct!"
    }
    else if ((c$right == 8) & (input$answer == "A")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 8) & (input$answer == "C")){
      "Sorry, please try again.\nHint: Look at the scale"
    }
    else if ((c$right == 9) & (input$answer == "A")){
      "Correct!"
    }   
    else if ((c$right == 9) & (input$answer == "B")){
      "Sorry, please try again.\nHint: please double check the seasonality."
    }
    else if ((c$right == 9) & (input$answer == "C")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 10) & (input$answer == "B")){
      "Correct!"
    }   
    else if ((c$right == 10) & (input$answer == "A")){
      "Sorry, please try again.\nHint: please double check the seasonality."
    }
    else if ((c$right == 10) & (input$answer == "C")){
      "Sorry, please try again.\nHint: Is the trend going up or going down?"
    }
    else if ((c$right == 11) & (input$answer == "B")){
      "Correct!"
    }
    else if ((c$right == 11) & (input$answer == "A")){
      "Sorry, please try again."
    }
    else if ((c$right == 11) & (input$answer == "C")){
      "Sorry, please try again."
    }
  })
  
}


# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
