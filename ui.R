library(shiny)
library(leaflet)
library(lubridate)
#library(dplyr)
library(shinythemes)

crime_data <- readRDS("data/crime_data.Rds")

ui <- fluidPage(#theme = shinytheme("cerulean"),
  includeCSS("style.css"),
  titlePanel("Cincinnati Crime Dashboard"),
  hr(),
  div(class="tabset",
  tabsetPanel(
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 h3("Data Filters"),
                 radioButtons("input_map_year",
                              "Year",
                              choices = c("All",sort(unique(year(crime_data$Occured.Date)))),
                              selected = "All",inline = TRUE),
                 sliderInput("input_map_CrimeHour",
                             label = "Time Range (Hour of the day)", min = 0, 
                             max = 23, value = c(0, 23)),
                 selectInput("input_map_CrimeType",
                             "Crime Type",
                             choices = c("All",sort(unique(crime_data$Crime.Group))),
                             selected = "All"),
                 hr(),
                 h3("Map Settings"),
                 sliderInput("input_map_circleSize", label = "Marker Size", min = 1, 
                             max = 10, value = 1),
                 sliderInput("input_map_circleWt", label = "Marker Weight", min = 1, 
                             max = 10, value = 2)
               ),
               mainPanel(
                 renderText(output$test),
                 leafletOutput("main_map",height = 600)
               )
             )
             
    ),
    
    tabPanel("Dashboard",
            sidebarLayout(
                  sidebarPanel(#style = "position:fixed; overflow: visible",
                    h3("Data Filters"),
                    sliderInput("input_analysis_CrimeHour",
                                label = "Time Range (Hour of the day)", min = 0, 
                                max = 23, value = c(0, 23)),
                    selectInput("input_analysis_CrimeType",
                                "Crime Type",
                                choices = c("All",sort(unique(crime_data$Crime.Group))),
                                selected = "All")
                    ),
                  mainPanel(
                    fluidRow(
                      splitLayout(cellWidths = c("29%", "70%"),
                                  plotOutput("yearly_graph",height=300),
                                  plotOutput("monthly_graph",height=300))
                    ),
                    plotOutput("hourly_graph",height=300),
                    plotOutput("street_graph",height=300),
                    div(class="error",("*Error in the above graph: Selected crime did not occur on any street across all three years")),
                    hr(),
                    div(class="table_header",("Streets list with increase/decrease in crime rates")),
                    dataTableOutput("street_list")
                  )
            )
    ),
    tabPanel("Data Source",
             p(" "),
             p("The data used for this project was publicly available at the location:
               https://data.cincinnati-oh.gov/Safer-Streets/Police-Crime-Incident-Data/."),
             hr(),
             p("This is the reported crimes data, which are collated by an agency for management and uploded. This
               data had serveral years worth of records. But for simplicity purposes, I filtered records only for
               2013, 2014 and 2015."),
             hr(),
             p("The GGMAP package was used to query the latitude and longitude of the crime address from Google Maps."),
             hr()
    )
  )),
  div(class="footer",("Project by Yash Sharma (yash91sharma@gmail.com)"))
)
