################
## Version notes
# v0.2 - Implemented new cases vs cumulative cases
# v0.3 - Fixed log scale labels
# v0.4 - Implemented ICM SIR modelling
################

## Required packages ----
library(DescTools)
library(dplyr)
library(drc)
library(EpiModel)
library(ggplot2)
library(httr)
library(lubridate)
library(plotly)
library(readxl)
library(reshape2)
library(rvest)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(stringr)
library(tidyr)
library(zoo)
set.seed(123)

## Required local functions ----
  ## Font ----
  font_new <- list(family = "arial")
  
  ## plotCases ----
  plotCases <- function(data, y, ylab, colour) {
    
    # Generate variable string
    if (y == "cases") {
      colnames(data)[which(colnames(data) == y)] <- "New cases"
      y <- "New cases"
    } else if (y == "deaths") {
      colnames(data)[which(colnames(data) == y)] <- "New deaths"
      y <- "New deaths"
    }
    
    # Create plot object
    p <- ggplot(data, aes(x = Date, y = !!sym(y)))+
      geom_point(alpha = 0.75, colour = colour)+
      geom_line(alpha = 0.75, colour = colour)+
      xlab("")+
      ylab(paste0("New ", ylab))+
      plotTheme(font_size = 12)#+
      #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 1))
    
    return(ggplotly(p) %>% layout(font = font_new))
    
  }
  
  ## plotCusumCases ----
  plotCusumCases <- function(data, y, scale, colour, limits_x, limits_y) {
    
    # Generate variable string
    y_var <- paste0("cusum_", y)
    y_var_log <- paste0("log_cusum_", y)
    lm_form <- as.formula(paste0(y_var_log, "~ Date"))
    drm_form <- as.formula(paste0(y_var, "~Date_num"))
    
    if (y == "cases") {
      data[["Cumulative cases"]] <- data[[y_var]]
      y_var <- "Cumulative cases"
    } else if (y == "deaths") {
       data[["Cumulative deaths"]] <- data[[y_var]]
      y_var <- "Cumulative deaths"
    }
    
    # Limits
    if (is.null(limits_x)) {
      limits_x <- c(min(data$Date), max(data$Date) + 5)
    }
    
    # Filter data 
    if (y == "cases") {
      data_model <- dplyr::filter(data, !!sym(y_var) >= 100)
    } else if (y == "deaths") {
      data_model <- dplyr::filter(data, !!sym(y_var) >= 10)
    }
    
    if (nrow(data_model) >= 5) {
      
      data_new <- data.frame(Date = data$Date + 5
                             ,Date_num = as.numeric(data$Date + 5))
      
      # Fit model and get predictions
      mean_last_cases <- tail(data_model[[paste0("increase_", y)]], n = 10)[is.finite(tail(data_model[[paste0("increase_", y)]], n = 10))] %>% median(na.rm = TRUE)
      #pearson_value <- cor(x = as.numeric(data_model$Date), y = data_model[[y_var_log]])
      
      if (mean_last_cases >= 1.1) {
        
        model <- lm(lm_form, data = data_model)
        data_pred <- as.data.frame(predict(model, data_new, interval = "prediction"))
        data_pred$Date <- data_new$Date
        data_pred <- data_pred %>%
          mutate(Predicted = round(10^fit, 0)
                 ,`Lower prediction interval` = round(10^lwr, 0)
                 ,`Upper prediction interval` = round(10^upr, 0))
        
      } else {
        
        data_model$Date_num <- as.numeric(data_model$Date)
        model <- drm(drm_form
                     ,data = data_model
                     ,fct = LL.4(
                       fixed = c(NA, 0, NA, NA)
                       ,names = c("Slope", "Min", "Max", "Inf_point")
                     )
        )
        data_pred <- as.data.frame(predict(model, data_new, interval = "prediction"))
        data_pred$Date <- data_new$Date
        data_pred$Date_num <- data_new$Date_num
        data_pred <- data_pred %>%
          mutate(Predicted = round(Prediction, 0)
                 ,`Lower prediction interval` = round(Lower, 0)
                 ,`Upper prediction interval` = round(Upper, 0))
        
      }
      
      max_cusum <- max(data_model[[y_var]])
      max_cusum_round <- ceiling(log10(max_cusum))
      
      
      # Create plot objects
      if (scale == "Linear") {
        
        p <- ggplot(data, aes(x = Date, y = !!sym(y_var)))+
          geom_line(data = data_pred, aes(y = Predicted), alpha = 0.4, colour = "red2")+
          geom_line(data = data_pred, aes(y = `Lower prediction interval`), alpha = 0.4, colour = "mediumorchid4")+
          geom_line(data = data_pred, aes(y = `Upper prediction interval`),alpha = 0.4, colour = "mediumorchid4")+
          geom_point(alpha = 0.75, colour = colour)+
          geom_line(alpha = 0.75, colour = colour)+
          # geom_text(x = min_date + 20, y = max_cusum, label = label)+
          xlab("")+
          ylab(paste0("Cumulative ", y, " (", tolower(scale), ")"))+
          plotTheme(font_size = 12)+
          #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 1))+
          xlim(limits_x)
        
      } else if (scale == "Logarithmic") {
        
        p <- ggplot(data, aes(x = Date, y = !!sym(y_var)))+
          geom_line(data = data_pred, aes(y = Predicted), alpha = 0.4, colour = "red2")+
          geom_line(data = data_pred, aes(y = `Lower prediction interval`), alpha = 0.4, colour = "mediumorchid4")+
          geom_line(data = data_pred, aes(y = `Upper prediction interval`),alpha = 0.4, colour = "mediumorchid4")+
          geom_point(alpha = 0.75, colour = colour)+
          geom_line(alpha = 0.75, colour = colour)+
          xlab("")+
          ylab(paste0("Cumulative ", y, " (", tolower(scale), ")"))+
          plotTheme(font_size = 12)+
          #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 1))+
          scale_y_log10(breaks = 10^(seq(0, max_cusum_round, length.out = max_cusum_round + 1)))+
          xlim(limits_x)
        
      }
      
    } else {
      
      # Create plot objects
      if (scale == "Linear") {
        
        p <- ggplot(data, aes(x = Date, y = !!sym(y_var)))+
          geom_point(alpha = 0.75, colour = colour)+
          geom_line(alpha = 0.75, colour = colour)+
          xlab("")+
          ylab(paste0("Cumulative ", y, " (", tolower(scale), ")"))+
          plotTheme(font_size = 12)+
          #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 1))+
          xlim(limits_x)
        
      } else if (scale == "Logarithmic") {
        
        p <- ggplot(data, aes(x = Date, y = !!sym(y_var)))+
          geom_point(alpha = 0.75, colour = colour)+
          geom_line(alpha = 0.75, colour = colour)+
          xlab("")+
          ylab(paste0("Cumulative ", y, " (", tolower(scale), ")"))+
          plotTheme(font_size = 12)+
          #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 1))+
          scale_y_log10()+
          xlim(limits_x)
        
      }
    }
    
    p <- ggplotly(p, dynamicTicks = TRUE) %>% layout(font = font_new)
    return(p)
    
  }
  
  ## plotMonitor ----
  plotMonitor <- function(data, y, colour) {
    
    # Generate variable string
    y_var <- paste0("cusum_", y)
    if (y == "cases") {
      colnames(data)[which(colnames(data) == y)] <- "New cases"
      colnames(data)[which(colnames(data) == y_var)] <- "Cumulative cases"
      y <- "New cases"
      y_var <- "Cumulative cases"
    } else if (y == "deaths") {
      colnames(data)[which(colnames(data) == y)] <- "New deaths"
      colnames(data)[which(colnames(data) == y_var)] <- "Cumulative deaths"
      y <- "New deaths"
      y_var <- "Cumulative deaths"
    }
    
    max_x <- ceiling(log10(max(data[[y]], na.rm = TRUE)))
    max_y <- ceiling(log10(max(data[[y_var]], na.rm = TRUE)))
    
    # Create plot object
    data <- data %>%
      dplyr::filter(!!sym(y) > 0) %>%
      dplyr::filter(!!sym(y_var) > 0)
    
    p <- ggplot(data, aes(x = !!sym(y_var), y = !!sym(y)))+
      stat_smooth(method = "loess", alpha = 0.75, fill = "grey90", size = 0.75)+
      geom_point(alpha = 0.75, colour = colour)+
      xlab(paste0(y_var, " (logarithmic)"))+
      ylab(paste0(y, " (logarithmic)"))+
      plotTheme(font_size = 12)+
      scale_x_log10(breaks = 10^(seq(0, max_x, length.out = max_x + 1)))+
      scale_y_log10(breaks = 10^(seq(0, max_y, length.out = max_y + 1)))
      
    p <- ggplotly(p, dynamicTicks = TRUE) %>% layout(font = font_new)
    return(p)
    
  }
  
  ## plotTheme ----
  plotTheme <- function(font_size) {
    return(ggplot2:: theme(panel.background = element_blank()
                           ,panel.grid.minor = element_blank()
                           ,panel.grid.major = element_blank()
                           ,axis.text = element_text(size = font_size, colour = "black")
                           ,axis.title = element_text(size = font_size, colour = "black")
                           ,panel.border = element_rect(fill = NA, colour = "black")
                           ,strip.background = element_rect(fill = "grey80", colour = "black")
                           ,strip.text = element_text(size = font_size, colour = "black")
                           ,legend.background = element_rect(fill = NA, colour = "black")
                           ,legend.title = element_text(face = "bold", size = font_size, colour = "black")
                           ,legend.text = element_text(size = font_size, colour = "black"))
    )
  }
  
## Read data ----
today_data_url <- "https://worldometers.info/coronavirus/"
data_url <- paste("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/")
df_main <- read.csv(data_url)
df_main$dateRep <- lubridate::dmy(df_main$dateRep)
colnames(df_main)[which(colnames(df_main) == "dateRep")] <- "Date"

## Header ----
header <- dashboardHeader(title = "COVID-19 dashboard")

## Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("UK data", tabName = "data_uk", icon = icon("chart-line"))
    ,menuItem("World data", tabName = "data_world", icon = icon("globe"))
    ,menuItem("Today's data", tabName = "data_today", icon = icon("calendar"))
    ,menuItem("SIR modelling", tabName = "modelling", icon = icon("chart-area"))
    ,menuItem("Information", tabName = "info", icon = icon("info-circle"))
  )
)

## Body ----
body <- dashboardBody(
  tags$style(
    HTML(
      ".box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#00a65a
        }
       .box.box-solid.box-primary {
          border-bottom-color:#00a65a;
          border-left-color:#00a65a;
          border-right-color:#00a65a;
          border-top-color:#00a65a;
       }"
    )
  )
  ,tabItems(
    
    ## First tab "UK data" ----
    tabItem(tabName = "data_uk"
            ,fluidRow(
              box(
                title = "Cumulative cases (UK)"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,status = "primary"
                ,plotlyOutput("plot_cusum_cases_uk")
                ,selectInput(
                  inputId = "cusum_axis_cases"
                  ,label = "Select the y-axis to show:"
                  ,choices = c("Linear", "Logarithmic")
                )
                ,uiOutput("sliders_cusum_cases")
                ,width = 6
              )
              ,box(
                title = "Cumulative deaths (UK)"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,status = "primary"
                ,plotlyOutput("plot_cusum_deaths_uk")
                ,selectInput(
                  inputId = "cusum_axis_deaths"
                  ,label = "Select the y-axis to show:"
                  ,choices = c("Linear", "Logarithmic")
                )
                ,uiOutput("sliders_cusum_deaths")
                ,width = 6
              )
            )
            ,fluidRow(
              box(
                title = "Case monitor (UK)"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,status = "primary"
                ,plotlyOutput("plot_monitor_cases_uk")
                ,width = 6
              )
              ,box(
                title = "Death monitor (UK)"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,status = "primary"
                ,plotlyOutput("plot_monitor_deaths_uk")
                ,width = 6
              )
            )
            ,fluidRow(
              box(
                title = "New cases (UK)"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,status = "primary"
                ,plotlyOutput("plot_new_cases_uk")
                ,width = 6
              )
              ,box(
                title = "New deaths (UK)"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,status = "primary"
                ,plotlyOutput("plot_new_deaths_uk")
                ,width = 6
              )
            )
    )
    
    ## Second tab "World data" ----
    ,tabItem(tabName = "data_world"
             ,fluidRow(
               box(
                 title = "Country selection"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,status = "primary"
                 ,uiOutput("country_select")
                 ,width = 3
                )
             )
             ,fluidRow(
               box(
                 title = "Cumulative cases"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,status = "primary"
                 ,plotlyOutput("plot_cusum_cases")
                 ,selectInput(
                   inputId = "cusum_axis_cases_country"
                   ,label = "Select the y-axis to show:"
                   ,choices = c("Linear", "Logarithmic")
                 )
                 ,uiOutput("sliders_cusum_cases_country")
                 ,width = 6
               )
               ,box(
                 title = "Cumulative deaths"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,status = "primary"
                 ,plotlyOutput("plot_cusum_deaths")
                 ,selectInput(
                   inputId = "cusum_axis_deaths_country"
                   ,label = "Select the y-axis to show:"
                   ,choices = c("Linear", "Logarithmic")
                 )
                 ,uiOutput("sliders_cusum_deaths_country")
                 ,width = 6
               )
             )
             ,fluidRow(
               box(
                 title = "Case monitor"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,status = "primary"
                 ,plotlyOutput("plot_monitor_cases")
                 ,width = 6
               )
               ,box(
                 title = "Death monitor"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,status = "primary"
                 ,plotlyOutput("plot_monitor_deaths")
                 ,width = 6
               )
             )
             ,fluidRow(
               box(
                 title = "New cases"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,status = "primary"
                 ,plotlyOutput("plot_new_cases")
                 ,width = 6
               )
               ,box(
                 title = "New deaths"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,status = "primary"
                 ,plotlyOutput("plot_new_deaths")
                 ,width = 6
               )
             )
    )
    
    ## Third tab "Today's data" ----
    ,tabItem(
      tabName = "data_today"
      ,fluidRow(
        box(
          title = "Today's data (source: Worldometer)"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,status = "primary"
          ,DT::dataTableOutput("data_raw")
          ,width = 12
        )
        ,column(
          width = 3
          ,actionButton(
            inputId = "refresh_button"
            ,label = "Refresh from Worldometer"
            ,icon = icon("sync-alt")
          )
        )
      )
    )
  
    ## Fourth tab "SIR modelling" ----
    ,tabItem(
      tabName = "modelling"
      ,fluidRow(
        box(
          title = "SIR model information"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,status = "primary"
          ,uiOutput("modelling_info")
          ,width = 12
        )
      )
      ,fluidRow(
        box(
          title = "Modelling options"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,status = "primary"
          ,uiOutput("modelling_options")
          ,width = 5
        )
        ,box(
          title = "Modelling output"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,status = "primary"
          ,plotlyOutput("modelling_output")
          ,width = 7
          ,height = 675
        )
      )
    )
    ## Fifth tab "Information" ----
    ,tabItem(
      tabName = "info"
      ,fluidRow(
        column(
          width = 9
          ,h2("General information")
          ,p()
          ,p(strong("Author:"), "Ed Wilkes")
          ,p(strong("Version:"), "0.3")
          ,p(strong("Date released:"), "2020/03/29")
          ,h2("Modelling")
          ,p(
            "This dashboard fits an ordinary least-squares (OLS) model to the log-transformed
              cases/deaths data. It only fits the model to days with cases >= 100 (or 
              deaths >= 10) in order to avoid the lag phase prior to exponential growth. The 
              +5 day predictions are purely for interest and may not reflect on-going policy
              changes in the relevant country. True incidence data follow a logistic curve and
              thus a four-parameter logistic regression model is fitted to the data from 
              countries whose increase in case numbers is slowing (i.e., they have passed
              the inflection point of the logistic growth). The log-log new cases/deaths vs 
              cumulative cases/deaths (monitor) plot best visualises the inflection point of
              case/death growth when it occurs."
          )
          ,h2("Data sources")
          ,p(
            "This dashboard pulls data from ecdc.europa.edu and Worldometer. Both of which
              are freely available at the addresses below:"
          )
          ,uiOutput("url_worldometer")
          ,uiOutput("url_europa")
          ,h2("Other resources")
          ,p(
            "Other excellent visualisations/analyses of world COVID-19 data are available here:"
          )
          ,uiOutput("url_ourworld")
          ,uiOutput("url_minutephysics")
          ,uiOutput("url_3blue1brown")
          ,uiOutput("url_timchurches")
        )
      )
    )
  ) # closes tabItems()
  
  ## Window sizing code ----
  ,tags$head(
    tags$script('
                var height = 0;
                var width = 0;
                $(document).on("shiny:connected", function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                $(window).resize(function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                '
    )
  )
) # closes dashboardBody()

## UI ----
ui <- dashboardPage(
  header
  ,sidebar
  ,body
  ,skin = "green"
)

## Server ----
server <- function(input, output, session) {
  
  ## General functions ----
  window_height <- reactive({ifelse(is.null(input$height), 0, as.numeric(input$height))})
  window_width <- reactive({ifelse(is.null(input$width), 0, as.numeric(input$width))})
  
  ## "UK data" tab ----
    # UK cases data
    df_uk <- reactive({
      df <- dplyr::filter(df_main, countriesAndTerritories == "United_Kingdom") %>%
        dplyr::arrange(Date) %>%
        dplyr::mutate(cusum_cases = cumsum(cases)
                      ,log_cusum_cases = log10(cusum_cases)
                      ,cusum_deaths = cumsum(deaths)
                      ,log_cusum_deaths = log10(cusum_deaths)
                      ,increase_cases = cusum_cases / lag(cusum_cases, n = 1)
                      ,increase_deaths = cusum_deaths / lag(cusum_deaths, n = 1))
      return(df)
    })
  
    # Slider inputs
    output$sliders_cusum_cases <- renderUI({
      
      min_date <- min(df_uk()$Date)
      max_date <- max(df_uk()$Date) + 5
      slider_x <- sliderInput(
        inputId = "slider_cusum_cases_x"
        ,min = min_date
        ,max = max_date
        ,label = "Choose your x-axis range:"
        ,step = 1
        ,value = c(min_date, max_date)
      )
      return(slider_x)
      
    })
      
    output$sliders_cusum_deaths <- renderUI({
      
      min_date <- min(df_uk()$Date)
      max_date <- max(df_uk()$Date) + 5
      slider_x <- sliderInput(
        inputId = "slider_cusum_deaths_x"
        ,min = min_date
        ,max = max_date
        ,label = "Choose your x-axis range:"
        ,step = 1
        ,value = c(min_date, max_date)
      )
      return(slider_x)
      
    })
    
    # Plot UK cases data
    output$plot_cusum_cases_uk <- renderPlotly({
      plotCusumCases(data = df_uk()
                     ,y = "cases"
                     ,colour = "darkorange1"
                     ,scale = input$cusum_axis_cases
                     ,limits_x = input$slider_cusum_cases_x)
    })
    
    output$plot_cusum_deaths_uk <- renderPlotly({
      plotCusumCases(data = df_uk()
                     ,y = "deaths"
                     ,colour = "darkorchid4"
                     ,scale = input$cusum_axis_deaths
                     ,limits_x = input$slider_cusum_deaths_x)
    })
    
    output$plot_monitor_cases_uk <- renderPlotly({
      plotMonitor(data = df_uk()
                  ,y = "cases"
                  ,colour = "darkorange1")
    })
    
    output$plot_monitor_deaths_uk <- renderPlotly({
      plotMonitor(data = df_uk()
                  ,y = "deaths"
                  ,colour = "darkorchid4")
    })
    
    output$plot_new_cases_uk <- renderPlotly({
      plotCases(data = df_uk(), y = "cases", ylab = "cases", colour = "darkorange1")
    })
  
    output$plot_new_deaths_uk <- renderPlotly({
      plotCases(data = df_uk(), y = "deaths", ylab = "deaths", colour = "darkorchid4")
    })
  
  ## "World data" tab ----
    # Country selectInput
    output$country_select <- renderUI({
      countries <- df_main$countriesAndTerritories
      select <- selectInput(
        inputId = "country_select_input"
        ,label = "Select the country to analyse:"
        ,multiple = FALSE
        ,choices = countries
      )
    })  
    
    # Chosen country's data
    df_country <- reactive({
      req(input$country_select_input)
      df <- dplyr::filter(df_main, countriesAndTerritories == input$country_select_input) %>%
        dplyr::arrange(Date) %>%
        dplyr::mutate(cusum_cases = cumsum(cases)
                      ,log_cusum_cases = log10(cusum_cases)
                      ,cusum_deaths = cumsum(deaths)
                      ,log_cusum_deaths = log10(cusum_deaths)
                      ,increase_cases = cusum_cases / lag(cusum_cases, n = 1)
                      ,increase_deaths = cusum_deaths / lag(cusum_deaths, n = 1)) 
      return(df)
    })
    
    # Slider inputs
    output$sliders_cusum_cases_country <- renderUI({
      
      req(input$country_select_input)
      min_date <- min(df_country()$Date)
      max_date <- max(df_country()$Date) + 5
      slider_x <- sliderInput(
        inputId = "slider_cusum_cases_x_country"
        ,min = min_date
        ,max = max_date
        ,label = "Choose your x-axis range:"
        ,step = 1
        ,value = c(min_date, max_date)
      )
      return(slider_x)
      
    })
    
    output$sliders_cusum_deaths_country <- renderUI({
      
      req(input$country_select_input)
      min_date <- min(df_country()$Date)
      max_date <- max(df_country()$Date) + 5
      slider_x <- sliderInput(
        inputId = "slider_cusum_deaths_x_country"
        ,min = min_date
        ,max = max_date
        ,label = "Choose your x-axis range:"
        ,step = 1
        ,value = c(min_date, max_date)
      )
      return(slider_x)
      
    })
    
    # Plot cases data
    output$plot_cusum_cases <- renderPlotly({
      req(input$country_select_input)
      plotCusumCases(data = df_country()
                     ,y = "cases"
                     ,colour = "darkorange1"
                     ,scale = input$cusum_axis_cases_country
                     ,limits_x = input$slider_cusum_cases_x_country)
    })
    
    output$plot_cusum_deaths <- renderPlotly({
      req(input$country_select_input)
      plotCusumCases(data = df_country()
                     ,y = "deaths"
                     ,colour = "darkorchid4"
                     ,scale = input$cusum_axis_deaths_country
                     ,limits_x = input$slider_cusum_deaths_x_country)
    })
    
    output$plot_monitor_cases <- renderPlotly({
      plotMonitor(data = df_country()
                  ,y = "cases"
                  ,colour = "darkorange1")
    })
    
    output$plot_monitor_deaths <- renderPlotly({
      plotMonitor(data = df_country()
                  ,y = "deaths"
                  ,colour = "darkorchid4")
    })
    
    output$plot_new_cases <- renderPlotly({
      req(input$country_select_input)
      plotCases(data = df_country(), y = "cases", ylab = "cases", colour = "darkorange1")
    })
    
    output$plot_new_deaths <- renderPlotly({
      req(input$country_select_input)
      plotCases(data = df_country(), y = "deaths", ylab = "deaths", colour = "darkorchid4")
    })
    
  ## "Today's data" tab ----
    # Worldometer data
    df_raw <- eventReactive(input$refresh_button, {
      data_tables <- today_data_url %>%
        xml2::read_html() %>%
        html_nodes("table")
      main_table <- html_table(data_tables)[1]
      return(main_table[[1]])
    })
    
    output$data_raw <- DT::renderDataTable(
      {df_raw()}
      ,extension = c("Buttons", "Scroller")
      ,options = list(scrollX = TRUE, scroller = TRUE, scrollY = 650)
      ,rownames = FALSE
    )
  
  ## "SIR modelling" tab ----
    # Information
    output$modelling_info <- renderUI({
      HTML("This dashboard allows you to play around with a relatively simple stochastic Susceptible-Infectious-Removed (SIR) model. This model makes several assumptions, including:
           (i) removed/recovered individuals are fully immune and cannot get re-infected;
           (ii) the birth/immigration rate is constant in the population; (iii) there are no 
           sub-groups within the population (obviously incorrect!), and is therefore only a crude 
           approximation of reality. Try playing around with the exposure rate (reduced with 
           social distancing), infection probability (reduced with good hand hygiene and
           PPE use), and infected death rate (reduced with improved treatment(s), not 
           overwhelming ITU capacity, etc.) to see what happens to the trajectory of the 
           epidemic.")
    })
    
    # Options
    output$modelling_options <- renderUI({
      
      slider_act_rate <- sliderInput(
        inputId = "chosen_act_rate"
        ,label = "Choose your exposure rate (per day):"
        ,min = 0
        ,max = 20
        ,value = 10
        ,step = 1
      )
      
      slider_inf_prob <- sliderInput(
        inputId = "chosen_inf_prob"
        ,label = "Choose your infection probability (% per exposure):"
        ,min = 0
        ,max = 50
        ,value = 5
        ,step = 1
      )
      
      slider_rec_rate <- sliderInput(
        inputId = "chosen_rec_rate"
        ,label = "Choose your recovery rate (% per day):"
        ,min = 0
        ,max = 50
        ,value = 5
        ,step = 1
      )
      
      slider_pop_death_rate <- sliderInput(
        inputId = "chosen_pop_death_rate"
        ,label = "Choose your death rate from natural causes (per day):"
        ,min = 0
        ,max = 0.5
        ,value = 0.025
        ,step = 0.025
      )
      
      slider_death_rate <- sliderInput(
        inputId = "chosen_death_rate"
        ,label = "Choose your death rate of infected individuals (% per day):"
        ,min = 0
        ,max = 50
        ,value = 5
        ,step = 1
      )
      
      slider_pop_size <- selectInput(
        inputId = "chosen_pop_size"
        ,label = "Choose your total population size to model:"
        ,choices = c(1000, 10000, 100000)
        ,multiple = FALSE
        ,selected = 1000
      )
      
      slider_inf_people <- sliderInput(
        inputId = "chosen_inf_people"
        ,label = "Choose the number of infected people at time = 0:"
        ,min = 0
        ,max = 50
        ,value = 1
        ,step = 1
      )
      
      slider_n_days <- sliderInput(
        inputId = "chosen_n_days"
        ,label = "Choose the number of days you wish to model:"
        ,min = 50
        ,max = 1000
        ,value = 100
        ,step = 50
      )
      
      warning_text <- tags$i(
        "NB: Increasing the number of days to model and the population size will
        drastically increase computation time!"
      )
      
      return(
        list(
          slider_act_rate
          ,slider_inf_prob
          ,slider_rec_rate
          ,slider_death_rate
          ,slider_inf_people
          ,slider_n_days
          ,slider_pop_size
          ,warning_text
        )
      )
      
    })
    
    # Modelling and plotting
    output$modelling_output <- renderPlotly({
      
      req(input$chosen_inf_prob
          ,input$chosen_rec_rate
          ,input$chosen_act_rate)
      control <- EpiModel::control.icm(type = "SIR", nsteps = input$chosen_n_days, nsims = 10)
      total_num <- as.numeric(input$chosen_pop_size) - input$chosen_inf_people
      init <- EpiModel::init.icm(s.num = total_num
                                 ,i.num = input$chosen_inf_people
                                 ,r.num = 0)
      param <- EpiModel::param.icm(inf.prob = input$chosen_inf_prob/100
                                   ,act.rate = input$chosen_act_rate
                                   ,rec.rate = input$chosen_rec_rate/100
                                   ,a.rate = (11.1/365)/1000
                                   ,ds.rate = (9.413/365)/1000
                                   ,di.rate = (input$chosen_death_rate/100)/1000
                                   ,dr.rate = (9.413/365)/1000)
      sim <- EpiModel::icm(param, init, control)
      df_all <- dplyr::bind_cols(
        reshape2::melt(sim$epi$s.num)
        ,as.data.frame(reshape2::melt(sim$epi$i.num)[,2])
        ,as.data.frame(reshape2::melt(sim$epi$r.num)[,2])
      ) 
      colnames(df_all)[2:4] <- c("S", "I", "R")
      df_all$Time <- 1:input$chosen_n_days
      df_all <- df_all %>%
        group_by(Time) %>%
        mutate(S = S / as.numeric(input$chosen_pop_size) * 100
               ,I = I / as.numeric(input$chosen_pop_size) * 100
               ,R = R / as.numeric(input$chosen_pop_size) * 100) %>%
        summarise(Susceptible = mean(S)
                  ,Susceptible_error = sd(S)/sqrt(10)
                  ,Infected = mean(I)
                  ,Infected_error = sd(I)/sqrt(10)
                  ,Removed = mean(R)
                  ,Removed_error = sd(R)/sqrt(10))
      max_I <- max(df_all$Infected)
      peak_time <- df_all$Time[which(df_all$Infected == max_I)][1]
      
      p <- ggplot(df_all, aes(x = Time))+
        geom_line(aes(y = Susceptible), colour = "dodgerblue2", alpha = 0.75)+
        geom_ribbon(aes(ymin = Susceptible - Susceptible_error, ymax = Susceptible + Susceptible_error)
                    ,fill = "dodgerblue2", alpha = 0.5)+
        geom_line(aes(y = Infected), colour = "darkorange1", alpha = 0.75)+
        geom_ribbon(aes(ymin = Infected - Infected_error, ymax = Infected + Infected_error)
                    ,fill = "darkorange1", alpha = 0.5)+
        geom_line(aes(y = Removed), colour = "forestgreen", alpha = 0.75)+
        geom_ribbon(aes(ymin = Removed - Removed_error, ymax = Removed + Removed_error)
                    ,fill = "forestgreen", alpha = 0.5)+
        geom_hline(yintercept = max_I, colour = "red2", alpha = 0.5, linetype = "dotted")+
        geom_vline(xintercept = peak_time, colour = "red2", alpha = 0.5, linetype = "dotted")+
        plotTheme(font_size = 12)+
        xlab("Time (days)")+
        ylab("Proportion of total population (%)")+
        theme(legend.position = "bottom")
      p <- ggplotly(p, height = 600) %>% 
        layout(font = font_new, legend = list(orientation = "h"))
      return(p)
      
    })
    
  ## "Information" tab ----
    output$url_europa <- renderUI({
      tagList(""
              ,a("ECDC Europa"
                 ,href = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/")
      )
    })
    
    output$url_worldometer <- renderUI({
      tagList(""
              ,a("Worldometer"
                 ,href = "https://worldometers.info/coronavirus/")
      )
    })
    
    output$url_ourworld <- renderUI({
      tagList(""
              ,a("Our World in Data"
                 ,href = "https://ourworldindata.org/coronavirus")
      )
    })
    
    output$url_minutephysics <- renderUI({
      tagList(""
              ,a("Minutephysics"
                 ,href = "https://www.youtube.com/user/minutephysics")
      )
    })
    
    output$url_3blue1brown <- renderUI({
      tagList(""
              ,a("3Blue1Brown"
                 ,href = "https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw")
      )
    })
    
    output$url_timchurches <- renderUI({
      tagList(""
              ,a("Tim Churches blog"
                 ,href = "https://timchurches.github.io/blog/")
      )
    })
}

## Call ----
shinyApp(ui, server)
