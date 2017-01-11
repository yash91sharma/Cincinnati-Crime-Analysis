library(shiny)
library(leaflet)
library(lubridate)
library(dplyr)

crime_data <- readRDS("data/crime_data.Rds")

ui <- fluidPage(
  includeCSS("style.css"),
  titlePanel("Cincinnati Crime Dashboard"),
  tabsetPanel(
      tabPanel("Analysis",
            sidebarLayout(
                  sidebarPanel(
                    h3("Crime Filters"),
                    sliderInput("input_analysis_CrimeHour",
                                label = "Time Range (Hour of the day)", min = 0, 
                                max = 23, value = c(0, 23)),
                    selectInput("input_analysis_CrimeType",
                                "Crime Type",
                                choices = c("All",sort(unique(crime_data$Crime.Group))),
                                selected = "All")
                    ),
                  
                  mainPanel(
                    plotOutput("yearly_graph",width = 400),
                    plotOutput("monthly_graph",width = 1000),
                    plotOutput("hourly_graph"),
                    plotOutput("street_graph")
                    
                  )
            )
    ),
    tabPanel("Map",
             sidebarLayout(
                  sidebarPanel(
                    h3("Crime Filters"),
                    radioButtons("input_map_year",
                                 "Year",
                                 choices = c("All",sort(unique(year(crime_data$Occured.Date)))),
                                 selected = "All",inline = TRUE),
                    radioButtons("input_map_CrimeTime", 
                                 "Time of the day",
                                 choices = c("All",sort(unique(crime_data$Occured.Time2))),
                                 selected = "All",inline = TRUE),
                    sliderInput("input_map_CrimeHour",
                                label = "Time Range (Hour of the day)", min = min(crime_data$crimeOccHour), 
                                max = max(crime_data$crimeOccHour), value = c(0, 23)),
                    selectInput("input_map_CrimeType",
                                "Crime Type",
                                choices = c("All",sort(unique(crime_data$Crime.Group))),
                                selected = "All"),
                    hr(),
                    h3("Map Settings"),
                    sliderInput("input_map_circleSize", label = "Marker Size", min = 1, 
                                max = 5, value = 1),
                    sliderInput("input_map_circleWt", label = "Marker Weight", min = 1, 
                                max = 5, value = 2)
                  ),
                  mainPanel(
                    renderText(output$test),
                    leafletOutput("main_map",height = 600),
                    "*Stratified subset of data being shown on the map to increase application performance"
                  )
             )
      
    ),
    tabPanel("Data Preparation"
      
    )
  )
)