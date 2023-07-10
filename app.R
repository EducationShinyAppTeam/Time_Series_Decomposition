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
            seasonality,  and random components of time series plot."),
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
          p("This app was developed and coded by Jiajun Gao and updated by Luqi 
            Jiao Emanuele in 2023.",
            br(),br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated",  "Last Update: 06/23/2023 by LJE.")
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
                    "S&P 500", "State College Weather", 
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
                  min = 0, max = 200, value = 0
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
          br(),
            fluidRow(
              column(
                width = 6,
                imageOutput(
                  "questiongraph", 
                  height = 250)
              ),
              column(
                width = 6,
                wellPanel(
                uiOutput(outputId = "questionDisplayed"),
                br(),
                actionButton(
                  inputId = "newchallenge",
                  label = "New Challenge",
                  size = "small",
                  style = "default"
                  ),
                br(),
                br(),
                selectInput("answer", "Select your answer", 
                            choices = list("A", "B", "C", ""), 
                            selected = ""),
                actionButton(
                  inputId = "submit",
                  label = "Submit",
                  size = "small",
                  style = "default"
                ),
                br(),
                br(),
                uiOutput("icon"),
                br(),
                uiOutput("response")
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
          labs(title = "Time Series Decomposition of the Unemployment Rate 
                 in the US",
               x = "Time", y = "Price ($)") +
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
    
    ### SC weather ----
    else if (input$dataset == "State College Weather") {
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
          labs(title = "Time Series Decomposition of State College Weather",
               x = "Time", y = "Temperature") +
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
        "This is the time series decomposition plot of the State College Weather 
        with time on the x-axis, and temperature on the y-axis." }
        else if (input$seeTrend == TRUE && input$seeSeasonal == FALSE) {
          "This is the time series decomposition plot of the State College Weather 
        with time on the x-axis, and temperature on the y-axis, and showing the 
          trend of the plot."
        }
        else if (input$seeSeasonal == TRUE && input$seeTrend == FALSE) {
          "This is the time series decomposition plot of the State College Weather 
        with time on the x-axis, and temperature on the y-axis, and showing the 
          seasonality of the plot."
        }
        else if (input$seeTrend == TRUE && input$seeSeasonal == TRUE) {
          "This is the time series decomposition plot of the State College Weather 
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
                y.sim2 = input$trend * t + y.sim + 
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
                  y.sim2 = input$trend * t + y.sim + 
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
                                       ma = c(0.5)), 
                          rand.gen = error.model)
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
                                       ma = c(0.5)), 
                          rand.gen = error.model)
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
                                       ma = c(0.5)), 
                          rand.gen = error.model)
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
                  output$simplot <- renderPlot(ts.plot(b1(), 
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
                  output$simplot <- renderPlot(ts.plot(c1(), 
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
  
  observeEvent(input$next2, {
    updateTabItems(session, "pages", "challenge")
  })
  
  
  ##Generate challenges ---- 
  c <- reactiveValues(right = c(sample(1:11,1)))
  
  ### Neil Changes
  ### Get new challenge and reset feedback ----
  observeEvent(
    eventExpr = input$newchallenge,
    handlerExpr = {
      c$right <- sample(x = 1:11, size = 1)
      
      updateSelectInput(
        session = session,
        inputId = "answer",
        selected = ""
      )
      
      output$icon <- renderIcon()
      
      output$response <- renderUI(NULL)
    }
  )
  
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
      output$sumbit <- renderIcon()
    }
  )
  
  
  output$question1<- renderText( {
    if (c$right == 1) {
      "Challenge: please choose the plot of long term trend of the following time 
      series plot"
    }
    else if (c$right == 2) {
      "Challenge: please choose the plot of seasonality of the following time series 
      plot"
    }
    else if (c$right == 3) {
      "Challenge: please choose the corresponding time series plot based on the 
      following decomposed plots"
    }
    else if (c$right == 4) {
      "Challenge: please choose the plot of long term trend of the following time 
      series plot"
    }
    else if (c$right == 5) {
      "Challenge: please choose the plot of seasonality of the following time series 
      plot"
    }
    else if (c$right == 6) {
      "Challenge: please choose the corresponding time series plot based on the 
      following decomposed plots"
    }
    else if (c$right == 7) {
      "Challenge: please choose the plot of long term trend of the following time 
      series plot"
    }
    else if (c$right == 8) {
      "Challenge: please choose the plot of long term trend of the following time 
      series plot"
    }
    else if (c$right == 9) { 
      "Challenge: please choose the corresponding time series plot based on the 
      following decomposed plots"
    }
    else if (c$right == 10) {
      "Challenge: please choose the corresponding time series plot based on the 
      following decomposed plots"
    }
    else if (c$right == 11) {
      "Challenge: please choose the plot of seasonality of the following time series
      plot"
    }
  })
  
  output$questiongraph <- renderImage({
    if (c$right == 1) {
      return(list(
        src = "q1.jpg"))
    }
    else if (c$right == 2) {
      return(list(
        src = "q2.jpg"))
    }
    else if (c$right == 3) {
      return(list(
        src = "q3.jpg"))
    }
    else if (c$right == 4) {
      return(list(
        src = "q4.jpg"))
    }
    else if (c$right == 5) {
      return(list(
        src = "q5.jpg"))
    }
    else if (c$right == 6) {
      return(list(
        src = "q6.jpg"))
    }
    else if (c$right == 7) {
      return(list(
        src = "q7.jpg"))
    }
    else if (c$right == 8) {
      return(list(
        src = "q8.jpg"))
    }
    else if (c$right == 9) {
      return(list(
        src = "q9.jpg"))
    }
    else if (c$right == 10) {
      return(list(
        src = "q10.jpg"))
    }
    else if (c$right == 11) {
      return(list(
        src = "q11.jpg"))
    }
  }, deleteFile = FALSE)
  
  output$choice1 <- renderImage({
    if (c$right == 1) {
      return(list(
        src = "answer1a.jpg"))
    }
    else if (c$right == 2) {
      return(list(
        src = "answer2a.jpg"))
    }
    else if (c$right == 3) {
      return(list(
        src = "answer3a.jpg"))
    }
    else if (c$right == 4) {
      return(list(
        src = "answer4a.jpg"))
    }
    else if (c$right == 5) {
      return(list(
        src = "answer5a.jpg"))
    } 
    else if (c$right == 6) {
      return(list(
        src = "answer6a.jpg"))
    }
    else if (c$right == 7) {
      return(list(
        src = "answer7a.jpg"))
    }
    else if (c$right == 8) {
      return(list(
        src = "answer8a.jpg"))
    }
    else if (c$right == 9) {
      return(list(
        src = "answer9a.jpg"))
    }
    else if (c$right == 10) {
      return(list(
        src = "answer10a.jpg"))
    }
    else if (c$right == 11) {
      return(list(
        src = "answer11a.jpg"))
    }
  }, deleteFile = FALSE)
  
  
  output$choice2 <- renderImage({
    if (c$right == 1) {
      return(list(
        src = "answer1b.jpg"))
    }
    else if (c$right == 2) {
      return(list(
        src = "answer2b.jpg"))
    }
    else if (c$right == 3) {
      return(list(
        src = "answer3b.jpg"))
    }
    else if (c$right == 4) {
      return(list(
        src = "answer4b.jpg"))
    }
    else if (c$right == 5) {
      return(list(
        src = "answer5b.jpg"))
    }
    else if (c$right == 6) {
      return(list(
        src = "answer6b.jpg"))
    }
    else if (c$right == 7) {
      return(list(
        src = "answer7b.jpg"))
    }
    else if (c$right == 8) {
      return(list(
        src = "answer8b.jpg"))
    }
    else if (c$right == 9) {
      return(list(
        src = "answer9b.jpg"))
    }
    else if (c$right == 10) {
      return(list(
        src = "answer10b.jpg"))
    }
    else if (c$right == 11) {
      return(list(
        src = "answer11b.jpg"))
    }
  }, deleteFile = FALSE)
  
  output$choice3 <- renderImage({
    if (c$right == 1) {
      return(list(
        src = "answer1c.jpg"))
    }
    else if (c$right == 2) {
      return(list(
        src = "answer2c.jpg"))
    }
    else if (c$right == 3) {
      return(list(
        src = "answer3c.jpg"))
    }
    else if (c$right == 4) {
      return(list(
        src = "answer4c.jpg"))
    }
    else if (c$right == 5) {
      return(list(
        src = "answer5c.jpg"))
    }
    else if (c$right == 6) {
      return(list(
        src = "answer6c.jpg"))
    }
    else if (c$right == 7) {
      return(list(
        src = "answer7c.jpg"))
    }
    else if (c$right == 8) {
      return(list(
        src = "answer8c.jpg"))
    }
    else if (c$right == 9) {
      return(list(
        src = "answer9c.jpg"))
    }
    else if (c$right == 10) {
      return(list(
        src = "answer10c.jpg"))
    }
    else if (c$right == 11) {
      return(list(
        src = "answer11c.jpg"))
    }
  }, deleteFile = FALSE)

  observeEvent(
    eventExpr = input$submit, 
    handlerExp = {
      
      correct <- (((c$right == 1) && (input$answer == "C")) ||
                   ((c$right == 2) && (input$answer == "C")) ||
                   ((c$right == 3) && (input$answer == "A")) ||
                   ((c$right == 4) && (input$answer == "B")) ||
                   ((c$right == 5) && (input$answer == "A")) ||
                   ((c$right == 6) && (input$answer == "B")) ||
                   ((c$right == 7) && (input$answer == "C")) ||
                   ((c$right == 8) && (input$answer == "B")) ||
                   ((c$right == 9) && (input$answer == "A")) ||
                   ((c$right == 10) && (input$answer == "B")) ||
                   ((c$right == 11) && (input$answer == "B")))
      hint1 <- (((c$right == 1) & (input$answer == "B")) ||
                  ((c$right == 3) & (input$answer == "C")) ||
                  ((c$right == 4) & (input$answer == "A")) ||
                  ((c$right == 6) & (input$answer == "A")) ||
                  ((c$right == 7) & (input$answer == "A")) ||
                  ((c$right == 7) & (input$answer == "B")) ||
                  ((c$right == 8) & (input$answer == "A")) ||
                  ((c$right == 9) & (input$answer == "C")) ||
                  ((c$right == 10) & (input$answer == "C"))) 
      hint2 <- (((c$right == 1) & (input$answer == "A")) ||
                  ((c$right == 4) & (input$answer == "C")) ||
                  ((c$right == 8) & (input$answer == "C")))
      hint3 <- (((c$right == 3) & (input$answer == "B")) ||
                  ((c$right == 6) & (input$answer == "C")) ||
                  ((c$right == 9) & (input$answer == "B")) ||
                  ((c$right == 10) & (input$answer == "A")))
    
    if (correct) {
      output$icon <- renderIcon(icon = "correct", width = 45) 
    } 
    else {output$icon <- renderIcon(icon = "incorrect", width = 45)}
    
    if (hint1) {
      output$response <- renderText("Hint: Is the trend going up or going down?")
    }
    else if (hint2) {
      output$response <- renderText("Hint: Look at the scale.")
    }  
    else if (hint3) {
      output$response <- renderText("Hint: please double check the seasonality.")
    }
    else {
      output$response <- renderText("")
    }
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)