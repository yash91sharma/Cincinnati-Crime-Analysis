library(shiny)
library(ggplot2)
library(leaflet)
library(lubridate)
library(dplyr)
library(tidyr)

crime_data <- readRDS("data/crime_data.Rds")

server <- function(input,output)
{
  map_circle_radius = 1
  map_circle_weight = 2
  
  #main map
  output$main_map <- renderLeaflet({ 
    leaflet() %>% 
    addTiles() %>% 
    setView(-84.5092756,39.1268475,zoom=13) %>%
      addCircleMarkers(data=crime_data[complete.cases(crime_data),],
                     lat=~lat, 
                     lng=~lon,popup=crime_data$Offense, 
                     weight=map_circle_weight,
                     radius=map_circle_radius)
  })

  observeEvent({input$input_map_CrimeTime 
                input$input_map_year
                input$input_map_CrimeType
                input$input_map_circleSize
                input$input_map_circleWt},
    {       var_time <- input$input_map_CrimeTime
            if(var_time == "All")
            {var_time <- unique(crime_data$Occured.Time2)}
            
            var_year <- input$input_map_year
            if(var_year == "All")
            {var_year <- unique(year(crime_data$Occured.Date))}
            
            var_crimeType <- input$input_map_CrimeType
            if(var_crimeType == "All")
            { var_crimeType <- unique(crime_data$Crime.Group)}
          
            map_circle_radius <- input$input_map_circleSize
            map_circle_weight <- input$input_map_circleWt
            
            main_map <- leafletProxy("main_map") 
            main_map %>% clearMarkers()
            
            crime_data_map_subset <- filter(crime_data[complete.cases(crime_data),],
                                        (Occured.Time2 %in% var_time) & 
                                          (year(Occured.Date) %in% var_year) &
                                          (Crime.Group %in% var_crimeType)
                                        )
            
            main_map %>%
              addCircleMarkers(data=crime_data_map_subset,
                               lat=~lat,
                               lng=~lon,
                               popup=crime_data_map_subset$Offense,
                               weight=map_circle_weight,
                               radius=map_circle_radius) 
    }
  )
  
  #yearly crimes graph
  output$yearly_graph <- renderPlot({
    
    var_crimeType <- input$input_analysis_CrimeType
    if(var_crimeType == "All")
    { var_crimeType <- unique(crime_data$Crime.Group)}
    
    var_crimeHour_min <- input$input_analysis_CrimeHour[1]
    var_crimeHour_max <- input$input_analysis_CrimeHour[2]
    
    crime_data_subset <- filter(crime_data,
                                (Crime.Group %in% var_crimeType) &
                                (var_crimeHour_max >= crimeOccHour) & (var_crimeHour_min <= crimeOccHour)
                                )
    
      ggplot(crime_data_subset,
             aes(x=as.factor(year(Occured.Date)),fill = as.factor(year(Occured.Date)) ))+
        geom_bar(stat="count",fill=c("rosybrown","orange", "royalblue3"))+
        geom_text(stat="count",aes(label=..count..),vjust = -1)+
        ggtitle("Yearly Crimes")+
        ylab("")+xlab("") +
        ylim(0,max(table(year(crime_data$Occured.Date)))+1800)
    }
  )
  
  #monthly trend graph
  output$monthly_graph <- renderPlot({
    
    var_crimeType <- input$input_analysis_CrimeType
    if(var_crimeType == "All")
    { var_crimeType <- unique(crime_data$Crime.Group)}
    
    var_crimeHour_min <- input$input_analysis_CrimeHour[1]
    var_crimeHour_max <- input$input_analysis_CrimeHour[2]
    
    crime_data_subset <- filter(crime_data,
                                (Crime.Group %in% var_crimeType) &
                                (var_crimeHour_max >= crimeOccHour) & (var_crimeHour_min <= crimeOccHour)
                                )
    
      ggplot(crime_data_subset,
             aes(x=as.factor(month(Occured.Date)),
                        group= as.factor(year(Occured.Date)),
                        colour = as.factor(year(Occured.Date)) ))+
        geom_line(stat="count",size=1.5)+
        scale_x_discrete(labels=month.abb)+
        ggtitle("Monthly Crime Trends")+
        ylab("")+
        ylim(0,max(table(month(crime_data$Occured.Date),year(crime_data$Occured.Date)))+100)+
        xlab("")+
        scale_colour_manual("Year", 
                            breaks = c("2013","2014", "2015"),
                            values = c("rosybrown","orange", "royalblue3"))

    
  })
  
  output$hourly_graph <- renderPlot({
    
    var_crimeType <- input$input_analysis_CrimeType
    if(var_crimeType == "All")
    { var_crimeType <- unique(crime_data$Crime.Group)}
    
    var_crimeHour_min <- input$input_analysis_CrimeHour[1]
    var_crimeHour_max <- input$input_analysis_CrimeHour[2]
    
    crime_data_subset <- filter(crime_data,
                                (Crime.Group %in% var_crimeType) &
                                (var_crimeHour_max >= crimeOccHour) & (var_crimeHour_min <= crimeOccHour)  
                                )
    
    ggplot(crime_data_subset,
           aes(as.factor(crimeOccHour)))+
      geom_histogram(stat="count")+
      scale_x_discrete(limits=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17",
                                "18","19","20","21","22","23"))+
      ggtitle("Crimes over the hours of day")+
      ylim(0,max(table(crime_data$crimeOccHour)))+
      xlab("")+ylab("")
  })
  
  output$street_graph <- renderPlot({
    var_crimeType <- input$input_analysis_CrimeType
    if(var_crimeType == "All")
    { var_crimeType <- unique(crime_data$Crime.Group)}
    
    var_crimeHour_min <- input$input_analysis_CrimeHour[1]
    var_crimeHour_max <- input$input_analysis_CrimeHour[2]
    
    crime_data_subset <- filter(crime_data,
                                (Crime.Group %in% var_crimeType) &
                                  (var_crimeHour_max >= crimeOccHour) & (var_crimeHour_min <= crimeOccHour)
                                )
    
    street_data <- crime_data_subset %>%
                  mutate(Street.Name = toupper(Street.Name)) %>%
                  group_by(Street.Name,year(crime_data_subset$Occured.Date)) %>%
                  summarise(count = n())
    
    colnames(street_data)[2] <- "year"
    street_data <- spread(street_data,year,count)
    street_data <- street_data[complete.cases(street_data),]
    street_data <- mutate(street_data,
                      Street.Type = ifelse( (`2015` < `2014`) & (`2014` < `2013`),"DECREASE",
                                            ifelse((`2015` > `2014`) & (`2014` > `2013`),"INCREASE",
                                                   ifelse((`2015` == `2014`) & (`2014` == `2013`),"CONSTANT","OTHER")
                                                   )
                                            )
                      )
    
    ggplot(street_data,aes(Street.Type))+
      geom_histogram(stat="count")+
      ylab("")+xlab("")+
      scale_x_discrete(limits=c("DECREASE","CONSTANT","INCREASE","OTHER"))
    
  })
  
}

