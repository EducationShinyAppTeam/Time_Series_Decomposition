# Load Package ----
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggfortify)
library(ggplot2)
library(zoo)

# Define global constants and load question banks ----
questionBank <- read.csv("questionBank.csv", header = TRUE)
questionBank <- na.omit(questionBank)

# Define UI for App ----

ui <- list(
  dashboardPage(
    skin = "purple",
    ## Header ----
    dashboardHeader(
      title = "Time Series Decomposition",
      titleWidth = 250,
      tags$li(
        class = "dropdown", 
        actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Assumptions_of_ANOVA"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "intro", icon = icon("tachometer-alt")),
        menuItem("Prerequisities", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "plots", icon = icon("wpexplorer")),
        menuItem("Simulation", tabName = "modify", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "challenge", icon = icon("cogs")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Create the content ----
    dashboardBody(
      tabItems(
        ### Set up the Overview Page ----
        tabItem(
          tabName = "intro",
          withMathJax(),
          h1("Time Series Decomposition"),
          br(),
          p("This application is to help you better understand long term trend, 
            seasonality,  and random components of time series plots."),
          h2("Instructions"),
          tags$ol(
            tags$li("You can explore time series plots for several different datasets 
                    from real life."),
            tags$li("Check the 'Trend' or 'Seasonal' box to look at those components 
                    of the series."),
            tags$li("In the Simulation section, you can modify trend, seasonality 
                    and random components and see how the simulated  time series 
                    plot changes."),
            tags$li("In the Challenge section, try to match the decompositions to 
                    the observed time series plot."),
            tags$li("Play with it and have fun!")
          ),
          br(),
          div(style = "text-align: center",
            bsButton(
              inputId = "start", 
              label = "Explore", 
              size = "large", 
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by Jiajun Gao in 2018. Stuart Vas 
            started updates in 2022 which Luqi Jiao Emanuele completed in 2023.",
            br(),br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated",  "Last Update: 07/20/2023 by LJE.")
          )
        ),
        ### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          h2("Time Series"),
          p("A time series refers to a sequence of data points or observations 
            collected and recorded over time, typically at regular intervals. It 
            is a fundamental concept in data analysis, where the focus is on studying 
            the patterns, trends, and behavior of data with respect to time."),
          p("Common Examples are:"),
          tags$ul(
            tags$li("Stock prices"),
            tags$li("Weather measurements"),
            tags$li("Economic indicators"),
            tags$li("Population statistics"),
            tags$li("Sensor readings")
          ),
          h3("Decomposition"),
          p("The app will focus on a time series decomposition that deconstructs 
            the series into the following components:"),
          tags$ol(
            tags$li("Trend: represents the long-term movement or direction of the 
                    time series."),
            tags$li("Seasonality: capturing periodic patterns that repeat at fixed 
                    intervals within the time series."),
            tags$li("Random components: representing the random or irregular fluctuations 
                    in the time series that cannot be attributed to the trend, 
                    seasonality, or other explanatory trends.")
          )
          
        ),
        ### Set up an Explore Page  ----
        tabItem(
          tabName = "plots",
          h2("Explore"),
          br(),
          p("There are five different datasets for you to explore the trend and 
            seasonality."),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                selectInput(
                  inputId = "dataset",
                  label = "Dataset",
                  choices = c(
                    "Ford Stock Price", "Unemployment Rate", 
                    "S&P 500", "State College Temperature", 
                    "GDP growth rate of U.S." ),
                  selected = "Ford Stock Price"
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
              bsButton(
                "nextpart", 
                "NEXT", 
                size = "large",
                center = TRUE
              )
            ),
            fluidRow(
              column(
                width = 7,
                uiOutput(outputId = "dataDesc"),
                br(),
                plotOutput("timeseriesplot"
                )
              )
            ),
            br(),
            div(style = "text-align: center",
                bsButton(
                  inputId = "start2", 
                  label = "Next", 
                  size = "large", 
                  style = "default"
                )
            )
          )
        ),
        ### Set up the Simulation Page ----
        tabItem(
          tabName = "modify",
          h2("Simulate Time Series Decomposition"),
          br(),
          h6("Use the toggle inputs to simulate time series plots with different
            inputs"),
          br(),
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
                  min = 0, max = 100, value = 0
                ),
                selectInput(
                  "simulation",
                  label = "Simulation",
                  choices = c("single process", "multiple processes"),
                  selected = "single process"
                ),
                ####add a conditonal slide
                conditionalPanel(
                  condition = "input.simulation == 'multiple processes'",
                  sliderInput("path", "# of paths", min = 1, max = 3, value = 1)
                ),
                conditionalPanel(
                  condition = "input.simulation",
                  style = "text-align: center" , 
                  bsButton(
                    "newpro", 
                    "New Process", 
                    size = "middle"
                  )
                )
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
          br(),
          div(style = "text-align: center",
              bsButton(
                inputId = "start3", 
                label = "Next", 
                size = "large", 
                style = "default"
              )
          )
        ),
        ### Set up the Challenge Page ----
        tabItem(
          tabName = "challenge",
          h2("Challenge"),
          p("Test your understanding by trying out these questions."),
          fluidRow(
            column(
              width = 7,
              uiOutput(outputId = "questionPlot"),
              # imageOutput(outputId = "extraOutput") #, height = 250)
            ),
            column(
              width = 5,
              wellPanel(
                # uiOutput(outputId = "question"),
                p("Review the three plots listed below and select the best one
                  that matches the question listed below the question plot."),
                br(),
                selectInput(
                  inputId = "response",
                  label = "Select your answer", 
                  choices = list("A", "B", "C", ""), 
                  selected = ""
                ),
                bsButton(
                  inputId = "submit",
                  label = "Submit",
                  size = "large",
                  style = "default"
                ),
                br(),
                br(),
                p("Feedback"),
                uiOutput("icon"),
                verbatimTextOutput("score"),
                uiOutput("answer"),
                bsButton(
                  inputId = "newChallenge",
                  label = "New Challenge",
                  size = "large",
                  style = "default"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              h4("option A"),
              uiOutput("choiceA")
              # imageOutput("choice1", height = 250)),
            ),
            column(
              width = 4,
              h4("option B"),
              uiOutput("choiceB")
              # imageOutput("choice2", height = 250)),
            ),
            column(
              width = 4,
              h4("option C"),
              uiOutput("choiceC")
              # imageOutput("choice3", height = 250))
            )
          )
        ),
        ### Set up the References Page-REQUIRED ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from 
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST Utilities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create
            dashboards with 'Shiny'. (v. 0.7.2). [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny: 
            Web application framework for R, R package. (v 1.7.1). [R package]. 
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(class = "hangingindent", 
            "Horikoshi M, Tang Y (2018). ggfortify: Data Visualization Tools for 
            Statistical Analysis Results. 
            https://CRAN.R-project.org/package=ggfortify."
          ), 
          p(class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2022), shinyWidgets: Custom 
            Inputs Widgets for Shiny. (v 0.7.0). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(class = "hangingindent",
            "Wickham, W. (2016), ggplot2: Elegant graphics for data analysis,
            R Package. Springer-Verlag New York. (v 3.3.6). [R package].
            Available from https://ggplot2.tidyverse.org"
          ),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)
# Define the server ----
server <- function(input, output, session) {
  observeEvent(
    eventExpr = input$info,{
      handlerExpr = {
        sendSweetAlert(
          session = session,
          title = "Instructions:",
          type = "info",
          closeOnClickOutside = TRUE,
          text = "Learn the different components of a time series plot. Also, 
      explore the components with real datasets, simulate a plot with different 
      controls of the components, and test yourself."
        )}
    })
  
  ## Start button ----
  observeEvent(
    eventExpr = input$start, {
      handlerExpr = { 
        updateTabItems(
          session = session, 
          inputId = "pages", 
          selected = "plots")}
    })
  
  observeEvent(
    eventExpr = input$start2, {
      handlerExpr = { 
        updateTabItems(
          session = session, 
          inputId = "pages", 
          selected = "modify")}
    })
  
  observeEvent(
    eventExpr = input$start3, {
      handlerExpr = { 
        updateTabItems(
          session = session, 
          inputId = "pages", 
          selected = "challenge")}
    })
  
  ## Explore plots ----
  ### Reset of the model line checkboxes 
  observeEvent(
    eventExpr = input$dataset, 
    handlerExpr = {
      updateCheckboxInput(
        session = session,
        inputId = "seeTrend",
        value = FALSE
      )
      
      updateCheckboxInput(
        session = session,
        inputId = "seeSeasonal",
        value = FALSE
      )
    }
    
  )
  observe({
    ### Ford plot ----
    if (input$dataset == "Ford Stock Price") {
      ford <- read.csv(file = "ford.csv")
      time = ford[[1]]
      price = ford[[5]]
      ts_ford = ts(price, frequency = 12, start = c(1988,7), end = c(2018,6))
      ford3 <- fortify(stats::decompose(ts_ford))
      
      ford3$trend_residue <- ford3$Data - ford3$trend
      ford3$se_residue <- ford3$Data - ford3$seasonal
      ford3$comb <- ford3$trend + ford3$seasonal
      
      output$timeseriesplot <- renderPlot(
        expr = {
        ford_plot <- ggplot(ford3, aes(Index)) +
          geom_line(data = ford3, 
                    aes(y = Data, color = "Original Series"),
                    linewidth = 1) +
                    labs(title = "Time Series Decomposition of Ford Stock Price",
                         x = "Time", y = "Price ($)") +
          scale_color_manual(
            name = "Components",
            values = c(
              "Original Series" = boastUtils::psuPalette[3],
              "Model" = boastUtils::psuPalette[1],
              "Residue" = boastUtils::boastPalette[2])
          ) +
          theme(
            text = element_text(size = 16))
        
        if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          ford_plot <- ford_plot + 
            geom_line(data = ford3, 
                      aes(y = trend, colour = "Model"),
                      linewidth = 1) +
            geom_line(data = ford3, 
                      aes(y = trend_residue, colour = "Residue"), 
                      linewidth = 1) 
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          ford_plot <- ford_plot + 
            geom_line(data = ford3,
                      aes(y = seasonal, color = "Model"),
                      linewidth = 1) +
            geom_line(data = ford3, 
                      aes(y = se_residue, colour = "Residue"), 
                      linewidth = 1)
        }
        
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          ford_plot <- ford_plot + 
            geom_line(data = ford3, 
                      aes(y = comb, color = "Model"),
                      linewidth = 1) +
            geom_line(data = ford3, 
                      aes(y = remainder, colour = "Residue"), 
                      linewidth = 1)
        }
        
        ford_plot
      },
      alt = {if (input$seeTrend == FALSE && input$seeSeasonal == FALSE) { 
        "This is the time series decomposition plot of the Ford Stock Price 
      with time on the x-axis, and price in dollars on the y-axis."}
      else if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
        "This is the time series decomposition plot of the Ford Stock Price 
        with time on the x-axis, and price in dollars on the y-axis, and showing 
        the trend of the plot."
      }
      else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
        "This is the time series decomposition plot of the Ford Stock Price 
        with time on the x-axis, and price in dollars on the y-axis, and showing 
        the seasonality of the plot."
      }
      else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
        "This is the time series decomposition plot of the Ford Stock Price 
        with time on the x-axis, and price in dollars on the y-axis, and showing 
        both trend and seasonality of the plot." 
      }
    })
  }
    
    ### Unemployment ----
    else if (input$dataset == "Unemployment Rate") {
      bh <- read.csv(file = "UNRATENSA.csv")
      time = bh[[1]]
      ur = bh[[2]]
      ts_bh = ts(ur, frequency = 12, start = c(1948,1), end = c(2021,6))
      unr_dec <- fortify(stats::decompose(ts_bh))
      unr_dec$ranless <- unr_dec$Data - unr_dec$remainder
      unr_dec$trend_residue <- unr_dec$Data - unr_dec$trend
      unr_dec$se_residue <- unr_dec$Data - unr_dec$seasonal
      unr_dec$comb <- unr_dec$trend + unr_dec$seasonal
      
      output$timeseriesplot <- renderPlot(
        expr = {
        unr_plot <- ggplot(unr_dec, aes(Index)) +
          geom_line(data = unr_dec, 
                    aes(y = Data, color = "Original Series"),
                    linewidth = 1) +
          labs(title = "Time Series Decomposition of the Unemployment Rate in the US",
               x = "Time", y = "Unempolyment Rate (%)") +
          scale_color_manual(name = "Components",
                             values = c(
                               "Original Series" = boastUtils::psuPalette[3],
                               "Model" = boastUtils::psuPalette[1],
                               "Residue" = boastUtils::boastPalette[2])
          ) +
          theme(text = element_text(size = 16))
        
        if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          unr_plot <- unr_plot + 
            geom_line(data = unr_dec,
                      aes(y = trend, colour = "Model"),
                      linewidth = 1) +
            geom_line(data = unr_dec, 
                      aes(y = trend_residue, colour = "Residue"), 
                      linewidth = 1) 
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          unr_plot <- unr_plot +
            geom_line( data = unr_dec,
                       aes(y = seasonal, color = "Model"),
                       linewidth = 1) +
            geom_line(data = unr_dec, 
                      aes(y = se_residue, colour = "Residue"), 
                      linewidth = 1)
        }
        
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          unr_plot <- unr_plot + 
            geom_line(data = unr_dec, 
                      aes(y = comb, color = "Model"),
                      linewidth = 1) +
            geom_line(data = unr_dec, 
                      aes(y = remainder, colour = "Residue"), 
                      linewidth = 1)
        }
        
        unr_plot
      },
      alt = {if (input$seeTrend == FALSE && input$seeSeasonal == FALSE) {
        "This is the time series decomposition plot of the Unemployment in the
        U.S. with time on the x-axis, and price in dollars on the y-axis."}
        else if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          "This is the time series decomposition plot of the Unemployment in the
        U.S. with time on the x-axis, and price in dollars on the y-axis, and showing 
        the trend of the plot."
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          "This is the time series decomposition plot of the Unemployment in the
        U.S. with time on the x-axis, and price in dollars on the y-axis, and showing 
        the seasonality of the plot."
        }
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          "This is the time series decomposition plot of the Unemployment in the
        U.S. with time on the x-axis, and price in dollars on the y-axis, and showing 
        both trend and seasonality of the plot." 
        }
    })
    }
    ### S&P 500 ---- 
    else if (input$dataset == "S&P 500") {
      bh <- read.csv(file = "sp500.csv")
      time = bh[[1]]
      price = bh[[5]]
      ts_sp = ts(price, frequency = 12, start = c(1988,1), end = c(2018,6))
      
      sp_dec <- fortify(stats::decompose(ts_sp))
      sp_dec$comb <- sp_dec$trend + sp_dec$seasonal
      sp_dec$trend_residue <- sp_dec$Data - sp_dec$trend
      sp_dec$se_residue <- sp_dec$Data - sp_dec$seasonal
      
      output$timeseriesplot <- renderPlot(
        expr = {
        sp_plot <- ggplot(sp_dec, aes(Index)) +
          geom_line(data = sp_dec, 
                    aes(y = Data, color = "Original Series"),
                    linewidth = 1) +
          labs(title = "Price of the Standard and Poor 500 Index price",
               x = "Time", y = "Price ($)") +
          scale_color_manual(
            name = "Components",
            values = c(
              "Original Series" = boastUtils::psuPalette[3],
              "Model" = boastUtils::psuPalette[1],
              "Residue" = boastUtils::boastPalette[2])
          ) +
          theme(text = element_text(size = 16))
        
        if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          sp_plot <- sp_plot + 
            geom_line(data = sp_dec, 
                      aes(y = trend, colour = "Model"),
                      linewidth = 1) +
            geom_line(data = sp_dec, 
                      aes(y = trend_residue, colour = "Residue"), 
                      linewidth = 1) 
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          sp_plot <- sp_plot +
            geom_line(data = sp_dec,
                      aes(y = seasonal, color = "Model"),
                      linewidth = 1) +
            geom_line(data = sp_dec, 
                      aes(y = se_residue, colour = "Residue"), 
                      linewidth = 1)
        }
        
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          sp_plot <- sp_plot + 
            geom_line(data = sp_dec, 
                      aes(y = comb, color = "Model"), 
                      linewidth = 1) +
            geom_line(data = sp_dec, 
                      aes(y = remainder, colour = "Residue"), 
                      linewidth = 1)
        }
        
        sp_plot
        
      },
      alt = { if (input$seeTrend == FALSE && input$seeSeasonal == FALSE) {
        "This is the time series decomposition plot of the price of the standard 
        and poor 500 index price with time on the x-axis, and price in dollars 
        on the y-axis." }
        else if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          "This is the time series decomposition plot of the price of the standard 
        and poor 500 index price with time on the x-axis, and price in dollars 
        on the y-axis, and showing the trend of the plot."
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          "This is the time series decomposition plot of the price of the standard 
        and poor 500 index price with time on the x-axis, and price in dollars 
        on the y-axis, and showing the seasonality of the plot."
        }
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          "This is the time series decomposition plot of the price of the standard 
        and poor 500 index price with time on the x-axis, and price in dollars 
        on the y-axis, and showing both trend and seasonality of the plot." 
        }
      })
    }
    
    ### SC temp ----
    else if (input$dataset == "State College Temperature") {
      sc <- read.csv(file = "StateCollegeWeather.csv")
      time = sc[[1]]
      temp = sc[[2]]
      ts_sc = ts(temp, frequency = 12, start = c(1988,1), end = c(2017,12))
      
      sc_dec <- fortify(stats::decompose(ts_sc))
      sc_dec$comb <- sc_dec$trend + sc_dec$seasonal
      sc_dec$trend_residue <- sc_dec$Data - sc_dec$trend
      sc_dec$se_residue <- sc_dec$Data - sc_dec$seasonal
      
      output$timeseriesplot <- renderPlot(
        expr = {
        sc_plot <- ggplot(sc_dec, aes(Index)) +
          labs(title = "Time Series Decomposition of State College Temperature",
               x = "Time", y = "Temperature (\u00B0F)") +
          geom_line(data = sc_dec, 
                    aes(y = Data, color = "Original Series"),
                    linewidth = 1) +
          scale_color_manual(name = "Components",
                             values = c(
                               "Original Series" = boastUtils::psuPalette[3],
                               "Model" = boastUtils::psuPalette[1],
                               "Residue" = boastUtils::boastPalette[2])
          ) +
          theme(text = element_text(size = 16))
        
        if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          sc_plot <- sc_plot + 
            geom_line(data = sc_dec, 
                      aes(y = trend, colour = "Model"), 
                      linewidth = 1) +
            geom_line(data = sc_dec, 
                      aes(y = trend_residue, colour = "Residue"), 
                      linewidth = 1) 
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          sc_plot <- sc_plot + 
            geom_line(data = sc_dec, 
                      aes(y = seasonal, color = "Model"), 
                      linewidth = 1) +
            geom_line(data = sc_dec, 
                      aes(y = se_residue, colour = "Residue"), 
                      linewidth = 1)
        }
        
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          sc_plot <- sc_plot + 
            geom_line(data = sc_dec, 
                      aes(y = comb, color = "Model"), 
                      linewidth = 1) +
            geom_line(data = sc_dec, 
                      aes(y = remainder, colour = "Residue"), 
                      linewidth = 1)
        }
        
        sc_plot
      },
      alt = {if (input$seeTrend == FALSE && input$seeSeasonal == FALSE) {
        "This is the time series decomposition plot of the State College Temperature 
        with time on the x-axis, and temperature on the y-axis." }
        else if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          "This is the time series decomposition plot of the State College Temperature 
        with time on the x-axis, and temperature on the y-axis, and showing the 
          trend of the plot."
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          "This is the time series decomposition plot of the State College Temperature 
        with time on the x-axis, and temperature on the y-axis, and showing the 
          seasonality of the plot."
        }
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          "This is the time series decomposition plot of the State College Temperature 
        with time on the x-axis, and temperature on the y-axis, and showing both 
          trend and seasonality of the plot." 
        }
      })
    }
    
    ### GDP growth rate of US ----
    else if (input$dataset == "GDP growth rate of U.S.") {
      sc <- read.csv(file = "gdp.csv")
      time = sc[[1]]
      gdp = sc[[2]]
      ts_gdp = ts(gdp, frequency = 4, start = c(1988,1), end = c(2018,1))
      gdp_dec <- fortify(stats::decompose(ts_gdp))
      gdp_dec$comb <- gdp_dec$trend + gdp_dec$seasonal
      gdp_dec$trend_residue <- gdp_dec$Data - gdp_dec$trend
      gdp_dec$se_residue <- gdp_dec$Data - gdp_dec$seasonal
      
      output$timeseriesplot <- renderPlot(
        expr = {
        gdp_plot <- ggplot(gdp_dec, aes(Index)) +
          labs(title = "Time Series Decomposition of the U.S. GDP Growth Rate",
               x = "Time", y = "GDP Growth Rate") +
          geom_line(data = gdp_dec, 
                    aes(y = Data, color = "Original Series"),
                    linewidth = 1) +
          scale_color_manual(
            name = "Components",
            values = c(
              "Original Series" = boastUtils::psuPalette[3],
              "Model" = boastUtils::psuPalette[1],
              "Residue" = boastUtils::boastPalette[2])
          ) +
          theme(text = element_text(size = 16))
        
        if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          gdp_plot <- gdp_plot + 
            geom_line(data = gdp_dec, 
                      aes(y = trend, colour = "Model"), 
                      linewidth = 1) +
            geom_line(data = gdp_dec, 
                      aes(y = trend_residue, colour = "Residue"), 
                      linewidth = 1) 
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          gdp_plot <- gdp_plot + 
            geom_line(data = gdp_dec, 
                      aes(y = seasonal, color = "Model"), 
                      linewidth = 1) +
            geom_line(data = gdp_dec, 
                      aes(y = se_residue, colour = "Residue"), 
                      linewidth = 1)
        }
        
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          gdp_plot <- gdp_plot + 
            geom_line(data = gdp_dec, 
                      aes(y = comb, color = "Model"), 
                      linewidth = 1) +
            geom_line(data = gdp_dec, 
                      aes(y = remainder, colour = "Residue"), 
                      linewidth = 1)
        }
        
        gdp_plot
        
      },
      alt = {if (input$seeTrend == FALSE && input$seeSeasonal == FALSE) {
        "This is the time series decomposition plot of the U.S. GDP Growth rate
        with time on the x-axis, and GDP growth rate on the y-axis." }
        else if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          "This is the time series decomposition plot of the U.S. GDP Growth rate
        with time on the x-axis, and GDP growth rate on the y-axis, and showing the 
          trend of the plot."
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          "This is the time series decomposition plot of the U.S. GDP Growth rate
        with time on the x-axis, and GDP growth rate on the y-axis, and showing 
          the seasonality of the plot."
        }
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          "This is the time series decomposition plot of the U.S. GDP Growth rate
        with time on the x-axis, and GDP growth rate on the y-axis, and showing both 
          trend and seasonality of the plot." 
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
        "Unemployment Rate"  = "The dataset displays the unemployment rate in
        the US from 1948 to 2021", 
        "S&P 500"  = "The data set records the price of 
        Standard and Poor 500 index from 1988 to 2018", 
        "State College Weather"  = "The dataset displays the temperature in
        State College from 1988 to 2017", 
        "GDP growth rate of U.S."  = "The dataset displays the GDP growth rate in
        the US from 1988 to 2018"
      )
      
      output$dataDesc <- renderUI(datasetDesc)
    }
  )
  
  ## Simulation ----
  observeEvent(input$nextpart, {
    updateTabItems(session, "pages", "modify")
  })
  
  observeEvent(
    input$simulation,{
      
      if (input$simulation == "single process") {
        updateSliderInput(session, "random", value = 0)
        updateSliderInput(session, "trend", value = 0)
        updateSliderInput(session, "season", value = 0)
        
        y.sim2 <- eventReactive(
          {input$random
            input$trend
            input$season}, {
              x <- input$random
              t = c(1:50)
              f <- input$season
              temp = 45
              error.model = function(x){rnorm(n = 50, sd = x, mean = 0)}
              set.seed(temp)
              y.sim = arima.sim(n = 50, list(ar = c(0.5), 
                                             ma = c(0.5)), 
                                rand.gen = error.model)
              if (input$season == 0 & input$random == 0) {
                y.sim2 = input$trend * t}
              else{
                y.sim2 = input$trend * t + 
                  rnorm(n = 50, sd = x, mean = 0) + 
                  f*cos(12*t)}
            }
        )
        output$simplot <- renderPlot(plot.ts(y.sim2(),
                                             ylab = "value",
                                             col = rgb(191, 116, 224, maxColorValue = 255),
                                             lwd = 2))
        
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
                t = c(1:50)
                
                f <- input$season
                set.seed(test)
                error.model = function(x){rnorm(n = 50, sd = x, mean = 0)}
                y.sim = arima.sim(n = 50, list(ar = c(0.5),
                                               ma = c(0.5)),
                                  rand.gen = error.model)
                if (input$season == 0 & input$random == 0) {
                  y.sim2 = input$trend * t}
                else{
                  y.sim2 = input$trend * t + 
                    rnorm(n = 50, sd = x, mean = 0) + 
                    f*cos(12*t)}
                
              }
          )
          output$simplot <- renderPlot(plot.ts(y.sim2(), 
                                               ylab = "value", 
                                               col = rgb(191, 116, 224, 
                                                         maxColorValue = 255), 
                                               lwd = 2))
        })
      }
      else if (input$simulation == "multiple processes") {
        updateSliderInput(session, "random", value = 0)
        updateSliderInput(session, "trend", value = 0)
        updateSliderInput(session, "season", value = 0)
        
        a1 <- eventReactive({
          input$random
          input$trend
          input$season},{
            x <- input$random
            t = c(1:50)
            f <- input$season
            error.model = function(x){rnorm(n = 50, sd = x, mean = 0)}
            a = arima.sim(n = 50, list(ar = c(0.5), 
                                       ma = c(0.5)))
            if (input$season == 0 & input$random == 0) {
              a1 = input$trend * t}
            else{
              a1 = input$trend * t + a + rnorm(n = 50, sd = x, mean = 0) + f*cos(12*t)}
          })
        b1 <- eventReactive({
          input$random
          input$trend
          input$season},{
            x <- input$random
            t = c(1:50)
            f <- input$season
            error.model = function(x){rnorm(n = 50, sd = x, mean = 0)}
            b = arima.sim(n = 50, list(ar = c(0.5), 
                                       ma = c(0.5)))
            if (input$season == 0 & input$random == 0) {
              b1 = input$trend * t}
            else{
              b1 = input$trend * t + b + rnorm(n = 50, sd = x, mean = 0) + f*cos(12*t)}
          })
        c1 <- eventReactive({
          input$random
          input$trend
          input$season},{
            x <- input$random
            t = c(1:50)
            f <- input$season
            error.model = function(x){rnorm(n = 50, sd = x, mean = 0)}
            c = arima.sim(n = 50, list(ar = c(0.5), 
                                       ma = c(0.5)))
            if (input$season == 0 & input$random == 0) {
              c1 = input$trend * t}
            else{
              c1 = input$trend * t + c + rnorm(n = 50, sd = x, mean = 0) + f*cos(12*t)}
            
          })
        
        ###  Path of multiple processes ----
        observeEvent(
          {input$path
            input$season
            input$random}, {
              if (input$path == 1) {
                output$simplot <- renderPlot(ts.plot(a1(), 
                                                     ylab = "value", 
                                                     lwd = 2, 
                                                     col = rgb(191, 116, 224, 
                                                               maxColorValue = 255)))
              }
              else if (input$path == 2) {
                if (input$season == 0 & input$random == 0) {
                  output$simplot <- renderPlot(ts.plot(a1(), 
                                                       ylab = "value", 
                                                       lwd = 2, 
                                                       col = rgb(99, 235, 235, 
                                                                 maxColorValue = 255)))
                }
                else{
                  output$simplot <- renderPlot(ts.plot(a1(), 
                                                       b1(), 
                                                       ylab = "value", 
                                                       lwd = 2, 
                                                       gpars = list(col = c(rgb(191, 116, 224,
                                                                                maxColorValue = 255), 
                                                                            rgb(99, 235, 235,
                                                                          maxColorValue = 255)))))
                  
                }
              }
              else if (input$path == 3) {
                if (input$season == 0 & input$random == 0) {
                  output$simplot <- renderPlot(ts.plot(a1(), 
                                                       ylab = "value", 
                                                       lwd = 2, 
                                                       col = rgb(255, 105, 180, 
                                                                 maxColorValue = 255)))
                }
                else{
                  output$simplot <- renderPlot(ts.plot(a1(),
                                                       b1(),
                                                       c1(), 
                                                       ylab = "value", 
                                                       lwd = 2, 
                                                       gpars = list(col = c(rgb(191, 116, 224, 
                                                                                maxColorValue = 255), 
                                                                            rgb(99, 235, 235, 
                                                                                maxColorValue = 255), 
                                                                            rgb(255, 105, 180, 
                                                                                maxColorValue = 255)))))
                }
              }
            })
        
      }
    })
  
  ## Challenge Page ----
  
  ## Proposed Change: let's build a question bank (CSV) file and put key 
  ## information there (prompt, images, alt text, hints, answers) so that we don't
  ## have such things hard coded into the app
  
  ### Create store of key elements for challenge page
  challengeElements <- reactiveValues(
    # Creates a vector of shuffled integers which we can use for the id column
    promptIds = sample(1:nrow(questionBank), size = nrow(questionBank), replace = FALSE),
    # Create current index
    currentIndex = 1,
    # Create a flag if current question has been answered
    answered = FALSE,
    # Create a flag for first visit to page
    firstTime = TRUE
  )
  
  ### Set watcher to iterate current question ----
  observeEvent(
    eventExpr = c(input$pages, input$newChallenge),
    handlerExpr = {
      if (input$pages == "challenge" & challengeElements$firstTime) {
        challengeElements$firstTime <- FALSE
      } else if (challengeElements$currentIndex == nrow(questionBank)) {
        sendSweetAlert(
          session = session,
          type = "error",
          title = "End of Game",
          text = "You've played through all of the questions."
        )
      } else {
        ## TO DO: Add commands to clear feedback first
        challengeElements$currentIndex <- challengeElements$currentIndex + 1
      }
    }
  )
  
  ### Display challenge plot and question ----
  scoreLevel <- reactiveVal(0)
  
  output$questionPlot <- renderUI(
    expr = {
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
        h3("Question Plot"),
        HTML(questionBank$extraOutput[questionId]),
        p(questionBank$question[questionId])
      )
    }
  )
  
  ### Display answer choices ----
  ##' TO DO: set up randomization so that the plots do not always appear in the 
  ##' same order. This will also necessitate editing the pictures so as to not
  ##' include "A", "B", and "C". We will also need to figure out how to track 
  ##' the correct graph for scoring
  
  random_order <- reactiveVal()
  
  random_choice <- function() {
    choices <- c("1", "2", "3")
    random_order <- sample(choices)
    return(random_order)
  }
  
  random_choice()
  
  observe({
    random_order_val <- random_order()
    
    output$choiceA <- renderUI({
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
        HTML(questionBank[[paste0("A")]][questionId])
      )
    })
    
    output$choiceB <- renderUI({
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
        HTML(questionBank[[paste0("B")]][questionId])
      )
    })
    
    output$choiceC <- renderUI({
      questionId <- challengeElements$promptIds[challengeElements$currentIndex]
      tagList(
        HTML(questionBank[[paste0("C")]][questionId])
      )
    })
  })
  
  
  
  current_question <- reactiveVal()
  
  random_question <- function() {
    random_index <- sample(nrow(questionBank), 1)
    current_question(questionBank[random_index, ])
  }
  
  random_question()
  
  output$question <- renderText({
    current_question()$question
  })
  
  output$extraOutput <- renderImage({
    if (!is.null(current_question()$extraOutput) && nchar(current_question()$extraOutput) > 0) 
      {
      return(list(src = current_question()$extraOutput))
    }
  },
  deleteFile = FALSE)
  
  output$A <- renderImage({
    if (!is.null(current_question()$A) && nchar(current_question()$A) > 0) 
    {
      return(list(src = current_question()$A))
    }
  },
  deleteFile = FALSE)
  
  output$B <- renderImage({
    if (!is.null(current_question()$B) && nchar(current_question()$B) > 0) 
    {
      return(list(src = current_question()$B))
    }
  },
  deleteFile = FALSE)
  
  output$C <- renderImage({
    if (!is.null(current_question()$C) && nchar(current_question()$C) > 0) 
    {
      return(list(src = current_question()$C))
    }
  },
  deleteFile = FALSE)
  
  observeEvent(
    eventExpr = input$submit, 
    handlerExpr = {
      user_answer <- input$response
      correct_answer <- current_question()$answer
      
      
      if (user_answer == correct_answer) {
        scoreLevel(scoreLevel() + 1)
        output$icon <- renderIcon("correct", width = 45)
      } else {
        scoreLevel(scoreLevel() + 0)
        output$icon <- renderIcon("incorrect", width = 45)
      }
      output$score <- renderText(
        expr = {
          paste("Total", scoreLevel(), "points.")
        }
      )
    }
  )

  ### Scoring ----
  output$scoreA <- renderText(
    expr = {
      paste("You have", scoreLevelA(), "points.")
    }
  )
  
  ### Get new challenge and reset feedback ----
  observeEvent(
    eventExpr = input$newChallenge,
    handlerExpr = {
      random_choice()
      random_question()
      scoreLevel()
      
      updateSelectInput(
        session = session,
        inputId = "answer",
        selected = ""
      )
      
      output$icon <- renderIcon()
      output$response <- renderUI(NULL)
      
    }
  )
  
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
## use ggplot to generate the simulation plots. 