# Load Package ----
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggfortify)
library(ggplot2)

# Define UI for App ----


# UI ----
ui <- list(
  dashboardPage(
    skin = "purple",
    ## Header ----
    dashboardHeader(
      title = "Time Series Decomposition",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "intro", icon = icon("tachometer-alt")),
        menuItem("Prerequisities", tabName = "prerequisites", icon = icon("book")),
        menuItem("Examples", tabName = "plots", icon = icon("book")),
        menuItem("Explore", tabName = "modify", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "challenge", icon = icon("cogs")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview ----
        tabItem(
          tabName = "intro",
          withMathJax(),
          h1("Time Series Decomposition"),
          br(),
          p("This application is to help you better understand 
             seasonality, long term trend and random components of time 
             series plot."),
          h2("Instructions"),
          tags$ol(
            tags$li("You can explore time series plots for several different 
                    datasets from real life."),
            tags$li("Check the 'decompose' box to look at seasonality, long 
                    term trend as well as random components separately."),
            tags$li("You can look at the information of each dataset by 
                    checking 'information' box."),
            tags$li("Then you can hit 'NEXT' to go to the simulate plots part."),
            tags$li("You can modify trend, seasonality and random components 
                    and see how the time series plot changes."),
            tags$li("Then you can hit 'NEXT' to go to the challenge part."),
            tags$li("In the challenge part, try to match the decompositions to 
                    the observed time series plot."),
            tags$li("Play with it and have fun!")
          ),
          br(),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "start", 
              label = "Explore", 
              size = "large", 
              icon = icon("bolt")
            )
          ),
          br(),br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by Jiajun Gao.",
            br(),br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),br(),
            div(class = "updated",  "Last Update: 00/00/2022 by NJH.")
          )
        ),
        ### Prerequisities ----
        tabItem(
          tabName = "prerequisites",
          h2("Prerequisites"),
          box(
            title = "What is time series decomposition?",
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Time Series Decomposition deconstructs a time series dataset into 
            different componenets to explore underlying patterns of the dataset"
          )
        ),
        ### Plots ----
        tabItem(
          tabName = "plots",
          h2("Explore"),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                selectInput(
                  inputId = "dataset",
                  label="Dataset",
                  choices = c(
                    "Ford Stock Price", "Unemployment Rate", 
                              "S&P 500", "State College Weather", 
                              "GDP growth rate of U.S." ),
                  selected = "Ford Stock Price"
                ),
                br(),
                selectInput(
                  inputId = "exampletype",
                  label = "Exploration type",
                  choices = c(
                    "Orginal Series", "Decompose"),
                  selected = "Original Series"
                ),
                conditionalPanel(
                  condition = "input.exampletype == 'Decompose'",
                  fluidRow(
                    checkboxInput(
                      inputId = "seeOriginal",
                      label = "Original Series",
                      value = FALSE
                    ),
                    checkboxInput(
                      inputId = "seeTrend",
                      label = "Trend",
                      value = FALSE
                    ),
                    checkboxInput(
                      inputId = "seeSeasonal",
                      label = "Seasonal",
                      value = FALSE
                    )
                  ),
                  p("Note: the original series line in the decomposition plot
                  does not contain the random part. Instead the random part 
                  is plotted as a separate line."),
                )
              ),
              # style = "text-align: center",
              bsButton(
                "nextpart", 
                "NEXT", 
                size = "large",
                center = TRUE
              )
            ),
            fluidRow(
              column(
                width =7,
                uiOutput(outputId = "dataDesc"),
                plotOutput(
                  "timeseriesplot", 
                  # height = 500
                ),
              )
            ),
            bsPopover("nextpart", " ", 
                      "Go to simulate plots.", 
                      place = "right", trigger = "hover"),
          )
        ),
        ### Simulate Plots ----
        tabItem(
          tabName = "modify",
          h2("Explore Time Series Decomposition"),
          h6("Use the toggle inputs to simulate time series plots with different
            inputs"),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = "trend", 
                  label = "slope of long term trend", 
                  min = -15, max = 15, value = 0
                ),
                sliderInput(
                  inputId = "season", 
                  label = "seasonality (amplitude)", 
                  min = 0, max = 200, value = 0
                ),
                sliderInput(
                  inputId = "random", 
                  label = "random error (s.d. of the error)", 
                  min = 0, max = 200, value = 0
                ),
                selectInput(
                  "simulation",
                  label="Simulation",
                  choices = c("single process", "multiple processes"),
                  selected = "single process"
                ),
                conditionalPanel(
                  condition = "input.simulation == 'single process'",
                  style = "text-align: center" , 
                  bsButton(
                    "newpro", 
                    "New Process", 
                    size = "middle", 
                  )
                ),
                ####add a conditonal slide
                conditionalPanel(
                  condition = "input.simulation == 'multiple processes'",
                  sliderInput("path", "# of paths", min = 1, max = 3, value = 1))
              )
            ),
            column(
              width = 8,
              wellPanel(
                style = "background-color: #EAF2F8",
                plotOutput("simplot", height = 500)
              )
            )
          ),
          br(),
          fluidRow(
            div(style = "text-align: center",
                bsButton("next2", "NEXT", size = "large")),
            bsPopover("next2", " ", 
                      "Go to the challenge part", 
                      place = "right", trigger = "hover"
            )
          )
        ),
        ### Challenge ----
        tabItem(
          tabName = "challenge",
          # verbatimTextOutput("question1"),
          h2("Challenge"),
          p("Test your understanding by trying out these questions"),
          wellPanel(
            fluidRow(
              column(
                width = 5,
                imageOutput(
                  "questiongraph", 
                  height = 280
                ),
              ),
              column(
                width = 5, 
                offset = 1,
                uiOutput(outputId = "questionDisplayed"),
                br(),br(),
                actionButton("newchallenge","New Challenge"),
                br(),br(),br(),
                selectInput("answer", "Select your answer", 
                            choices = list("A", "B", "C", ""), 
                            selected = "")
              )
            ),
            br(),
            fluidRow(
              column(
                width = 4, 
                offset = 6,
                verbatimTextOutput("response")
              )
            )
          ),
          wellPanel(
            fluidRow(
              column(
                width = 4,
                imageOutput("choice1", height = 250)),
              column(
                width = 4,
                imageOutput("choice2", height = 250)),
              column(
                width = 4,
                imageOutput("choice3", height = 250))
            )
          )
        ),
        ### References ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from 
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST Utilities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create
            dashboards with 'Shiny'. (v. 0.7.2). [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny: 
            Web application framework for R, R package. (v 1.7.1). [R package]. 
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent", 
            "Neudecker, A. (2021), shinyMatrix: Shiny matrix input field. (v 0.6.0).
            [R package]. Available from https://CRAN.R-project.org/package=shinyMatrix"
          ), 
          p(
            class = "hangingindent",
            "Novomestky, F. (2021, matrixcalc: Collection of functions for 
            matrix calculations. (v 1.0-5). [R package]. Available from
            https://CRAN.R-project.org/package=matrixcalc"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2022), shinyWidgets: Custom 
            Inputs Widgets for Shiny. (v 0.7.0). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016), ggplot2: Elegant graphics for data analysis,
            R Package. Springer-Verlag New York. (v 3.3.6). [R package].
            Available from https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2022). DT: A wrapper of the
            JavaScript library 'DataTables'. (v 0.23). [R package]. Available 
            from https://CRAN.R-project.org/package=DT"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
  
)
# Server implimentation ----
server <- function(input, output, session) {
  ## Start button ----
  observeEvent(input$start, {
    updateTabItems(session, "tabs", "plots")
  })
  ## Example plots ----
  observe({
    ### Ford plot ----
    if (input$dataset == "Ford Stock Price"){
      ford <- read.csv(file = "ford.csv")
      time = ford[[1]]
      price = ford[[5]]
      ts_ford = ts(price, frequency = 12, start = c(1988,7), end = c(2018,6))
      ford3 <- fortify(stats::decompose(ts_ford))
      ford3$ranless <- ford3$Data - ford3$remainder
      
      output$timeseriesplot <- renderPlot({
        if (input$exampletype == "Orginal Series") {
          plot.ts(ts_ford, ylab = "price")
        }
        else {
          ford_plot <- ggplot(ford3, aes(Index)) +
            geom_line(aes(y = remainder, color = "Random part"), size = 1) +
            labs(title = "Time Series Decomposition of Ford Stock Price",
                 x = "Time", y = "Price ($)") +
            scale_color_manual(
              name = "Components",
              values = c(
                "Original Series" = boastUtils::psuPalette[3],
                "Seasonal" = boastUtils::psuPalette[7],
                "Random part" = boastUtils::psuPalette[5],
                "Trend" = boastUtils::psuPalette[4]
              )
            )
          if (input$seeOriginal == TRUE) {
            ford_plot <- ford_plot + geom_line(data = ford3, 
                                               aes(y = ranless, color = "Original Series"), 
                                               size = 1) 
          }
          if (input$seeTrend == TRUE) {
            ford_plot <- ford_plot + geom_line(data = ford3, 
                                               aes(y = trend, colour = "Trend"), 
                                               size = 1)
          }
          if (input$seeSeasonal == TRUE) {
            ford_plot <- ford_plot + geom_line(data = ford3, 
                                               aes(y = seasonal, color = "Seasonal"), 
                                               size = 1)
          }
          ford_plot
        }
      })
        
    }
    
    ### Unemployment ----
    else if (input$dataset == "Unemployment Rate"){
      bh <- read.csv(file = "UNRATENSA.csv")
      time = bh[[1]]
      ur = bh[[2]]
      ts_bh = ts(ur, frequency = 12, start = c(1948,1), end = c(2021,6))
      unr_dec <- fortify(stats::decompose(ts_bh))
      unr_dec$ranless <- unr_dec$Data - unr_dec$remainder
      
      output$timeseriesplot <- renderPlot({
        if (input$exampletype == "Orginal Series") {
          plot.ts(ts_bh, ylab = "Unemployment Rate")
        }
        else {
          unr_plot <- ggplot(unr_dec, aes(Index)) +
            geom_line(aes(y = remainder, color = "Random part"), size = 1) +
            labs(title = "Time Series Decomposition of the Unemployment Rate 
                 in the US",
                 x = "Time", y = "Price ($)") +
          scale_color_manual(
            name = "Components",
            values = c(
              "Original Series" = boastUtils::boastPalette[1],
              "Seasonal" = boastUtils::boastPalette[5],
              "Random part" = boastUtils::boastPalette[2],
              "Trend" = boastUtils::boastPalette[3]
            )
          )
          if (input$seeOriginal == TRUE) {
            unr_plot <- unr_plot + geom_line(data = unr_dec, 
                                               aes(y = ranless, color = "Original Series"), 
                                               size = 1) 
          }
          if (input$seeTrend == TRUE) {
            unr_plot <- unr_plot + geom_line(data = unr_dec, 
                                               aes(y = trend, colour = "Trend"), 
                                               size = 1)
          }
          if (input$seeSeasonal == TRUE) {
            unr_plot <- unr_plot + geom_line(data = unr_dec, 
                                               aes(y = seasonal, color = "Seasonal"), 
                                               size = 1)
          }
          unr_plot
        }
      })
    }
    
    else if (input$dataset == "S&P 500"){
      bh <- read.csv(file = "sp500.csv")
      time = bh[[1]]
      price = bh[[5]]
      ts_sp = ts(price, frequency = 12, start = c(1988,1), end = c(2018,6))
      #output$timeseriesplot <- renderPlot(plot.ts(ts_sp, ylab = " "))
      
      sp_dec <- fortify(stats::decompose(ts_sp))
      sp_dec$ranless <- sp_dec$Data - sp_dec$remainder
      
      output$timeseriesplot <- renderPlot({
        if (input$exampletype == "Orginal Series") {
          plot.ts(ts_sp, ylab = "Price ($)")
        }
        else {
          sp_plot <- ggplot(sp_dec, aes(Index)) +
            geom_line(aes(y = remainder, color = "Random part"), size = 1) +
            labs(title = "Price of the Standard and Poor 500 Index price",
                 x = "Time", y = "Price ($)") +
            scale_color_manual(
              name = "Components",
              values = c(
                "Original Series" = boastUtils::boastPalette[1],
                "Seasonal" = boastUtils::boastPalette[5],
                "Random part" = boastUtils::boastPalette[2],
                "Trend" = boastUtils::boastPalette[3]
              )
            )
          if (input$seeOriginal == TRUE) {
            sp_plot <- sp_plot + geom_line(data = sp_dec, 
                                             aes(y = ranless, color = "Original Series"), 
                                             size = 1) 
          }
          if (input$seeTrend == TRUE) {
            sp_plot <- sp_plot + geom_line(data = sp_dec, 
                                             aes(y = trend, colour = "Trend"), 
                                             size = 1)
          }
          if (input$seeSeasonal == TRUE) {
            sp_plot <- sp_plot + geom_line(data = sp_dec, 
                                             aes(y = seasonal, color = "Seasonal"), 
                                             size = 1)
          }
          sp_plot
        }
      })
    }
    
    ### SCE weather ----
    else if (input$dataset == "State College Weather"){
      sc <- read.csv(file = "StateCollegeWeather.csv")
      time = sc[[1]]
      temp = sc[[2]]
      ts_sc = ts(temp, frequency = 12, start = c(1988,1), end = c(2017,12))
      #output$timeseriesplot <- renderPlot(plot.ts(ts_sc, ylab = "temperature"))
      
      sc_dec <- fortify(stats::decompose(ts_sc))
      sc_dec$ranless <- sc_dec$Data - sc_dec$remainder
      
      output$timeseriesplot <- renderPlot({
        if (input$exampletype == "Orginal Series") {
          plot.ts(ts_sc, ylab = "Temperature")
        }
        else {
          sc_plot <- ggplot(sc_dec, aes(Index)) +
            geom_line(aes(y = remainder, color = "Random part"), size = 1) +
            labs(title = "Time Series Decomposition of State College Weather",
                 x = "Time", y = "Temperature") +
          scale_color_manual(
            name = "Components",
            values = c(
              "Original Series" = boastUtils::boastPalette[1],
              "Seasonal" = boastUtils::boastPalette[5],
              "Random part" = boastUtils::boastPalette[2],
              "Trend" = boastUtils::boastPalette[3]
            )
          )
          if (input$seeOriginal == TRUE) {
            sc_plot <- sc_plot + geom_line(data = sc_dec, 
                                               aes(y = ranless, color = "Original Series"), 
                                               size = 1) 
          }
          if (input$seeTrend == TRUE) {
            sc_plot <- sc_plot + geom_line(data = sc_dec, 
                                               aes(y = trend, colour = "Trend"), 
                                               size = 1)
          }
          if (input$seeSeasonal == TRUE) {
            sc_plot <- sc_plot + geom_line(data = sc_dec, 
                                               aes(y = seasonal, colour = "Seasonal"), 
                                               size = 1)
          }
          sc_plot
        }
      })
    }
    
    else if (input$dataset == "GDP growth rate of U.S."){
      sc <- read.csv(file = "gdp.csv")
      time = sc[[1]]
      gdp = sc[[2]]
      ts_gdp = ts(gdp, frequency = 4, start = c(1988,1), end = c(2018,1))
      #output$timeseriesplot <- renderPlot(plot.ts(ts_gdp, ylab = "percent change"))
      
      gdp_dec <- fortify(stats::decompose(ts_gdp))
      gdp_dec$ranless <- gdp_dec$Data - gdp_dec$remainder
      
      output$timeseriesplot <- renderPlot({
        if (input$exampletype == "Orginal Series") {
          plot.ts(ts_gdp, ylab = "Price($)")
        }
        else {
          gdp_plot <- ggplot(gdp_dec, aes(Index)) +
            geom_line(aes(y = remainder, color = "Random part"), size = 1) +
            labs(title = "Time Series Decomposition of State College Weather",
                 x = "Time", y = "Temperature") +
            scale_color_manual(
              name = "Components",
              values = c(
                "Original Series" = boastUtils::boastPalette[1],
                "Seasonal" = boastUtils::boastPalette[5],
                "Random part" = boastUtils::boastPalette[2],
                "Trend" = boastUtils::boastPalette[3]
              )
            )
          if (input$seeOriginal == TRUE) {
            gdp_plot <- gdp_plot + geom_line(data = gdp_dec, 
                                           aes(y = ranless, color = "Original Series"), 
                                           size = 1) 
          }
          if (input$seeTrend == TRUE) {
            gdp_plot <- gdp_plot + geom_line(data = gdp_dec, 
                                           aes(y = trend, colour = "Trend"), 
                                           size = 1)
          }
          if (input$seeSeasonal == TRUE) {
            gdp_plot <- gdp_plot + geom_line(data = gdp_dec, 
                                           aes(y = seasonal, colour = "Seasonal"), 
                                           size = 1)
          }
          gdp_plot
        }
      })
    }
  })
  ### Data Description for Explore Plots ----
  
  observeEvent(
    eventExpr = input$dataset,
    handlerExpr = {
      datasetDesc <- switch(
        EXPR = input$dataset,
        "Ford Stock Price" = "The data set records the stock price of 
        Ford Motor Company from 1987 to 2017", 
        "Berkshire Hathaway Stock Price"  = "Insert Context here A", 
        "S&P 500"  = "Insert Context here B", 
        "State College Weather"  = "Insert Context here C", 
        "GDP growth rate of U.S."  = "Insert Context here D"
      )
      
      output$dataDesc<- renderUI(datasetDesc)
    }
  )
  
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
  
  observeEvent(
    eventExpr = c(input$newchallenge, c),
    handlerExpr = {
      questionout <- switch(
        c$right,
        "Choose the plot of seasonality of the 
        following time series plot",
        "Choose the plot of seasonality of the following 
        time series plot",
        "Choose the corresponding time series plot based 
        on the following decomposed plots",
        "Choose the plot of long term trend of the 
        following time series plot",
        "Choose the plot of seasonality of the following 
        time series plot",
        "Choose the corresponding time series plot based 
        on the following decomposed plots",
        "Choose the plot of long term trend of the following 
        time series plot",
        "Choose the plot of long term trend of the following time series 
        plot",
        "Choose the corresponding time series plot based 
        on the following decomposed plots",
        "Choose the corresponding time series plot based on the following 
        decomposed plots",
        "Choose the plot of seasonality of the following time series plot"
      )
      output$questionDisplayed <- renderUI(questionout)
    }
  )
  
  
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
