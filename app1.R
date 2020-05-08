library(shiny)
library(shinythemes)
library(rgdal)
library(plyr)
library(leaflet)
library(dplyr)
library(data.table)
library(DT)
library(car)
library(tidyverse)
library(MASS)
require(ggplot2)
library(plotly)
library(shinycustomloader)
library(shinydashboard)
library(leaflet.extras)



airport<-read.csv("airports.csv")
airline<-read.csv("airlines.csv")

# net_flights<-read.csv("net_flights.csv")


taxiInA<-subset(net_flights,!is.na(net_flights$TAXI_IN),
                select=c(TAXI_IN, AIRLINE_NAME)) %>%
                group_by(AIRLINE_NAME) %>%
                dplyr::summarise(mean_taxiIn=mean(TAXI_IN), count_of_fly=n()) %>%
                arrange(AIRLINE_NAME)




server <- function(input, output) {
  
  #-------------------
  # DIfferent delays 
  #-------------------
  
  
  map<-leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addCircleMarkers(~LONGITUDE, ~LATITUDE, 
                     data = airport, 
                     color = "purple", 
                     radius = 2, 
                     weight = 1,
                     popup = paste("Airport:", 
                                   airport$AIRPORT, 
                                   "<br>",
                                   "IATA_CODE:", 
                                   airport$IATA_CODE,
                                   "<br>",
                                   "CITY:", 
                                   airport$CITY)) 
  
  output$map <- renderLeaflet(map)

  #-------------------
  # DIfferent delays 
  #-------------------

  airport_src<-reactive({
    
    df_use_2 <- subset(net_flights, DESTINATION_AIRPORT == input$airport_sel2)
    
    df_use_2$status<-'ON TIME'
    df_use_2$status[df_use_2$DEPARTURE_DELAY>0] <-'DEPARTURE DELAY'
    df_use_2$status[df_use_2$CANCELLED==1] <-'CANCELLED FLIGHTS'
    df_use_2$status[df_use_2$ARRIVAL_DELAY>0 & (df_use_2$ARRIVAL_DELAY-df_use_2$DEPARTURE_DELAY)>0 & df_use_2$DEPARTURE_DELAY>0] <-'DEPR & ARVL_DELAY'
    df_use_2$status[df_use_2$ARRIVAL_DELAY>0 & df_use_2$DEPARTURE_DELAY<=0] <-'ARRIVAL_DELAY'
    df_use_2$status<-factor(df_use_2$status)
    
    
    return(df_use_2)
    
  })
  
  
  output$plot_delay_airlines  <- renderPlotly({
    
    ggplotly(ggplot(airport_src(),aes(x=AIRLINE,fill=status),binwidth=12) + 
               geom_bar(position = "fill") + 
               ggtitle("Flights by Airline According to Flights Status") +
              ylab('Percentage Mix'))
    
  })
  
  
  
  
  #-------------------
  # GGPlot 
  #-------------------
  
  pre_sub_flight<-reactive({
    
    df_pre <- net_flights[which(net_flights$DAY_OF_WEEK_F %in% input$day_week), ]
    
    return(df_pre)
    
    
  })
  
  
  sub_flight<-reactive({
    
    new_net <- pre_sub_flight()
    
    if (input$fixed == 1) {
      df_use <- subset(new_net, DESTINATION_AIRPORT_NAME == input$dest_airport_sel,
                       select=c(AIRLINE, ARRIVAL_DELAY, DEPARTURE_TIME))
    } else {
      df_use1 <- subset(new_net, DESTINATION_AIRPORT_NAME == input$dest_airport_sel)
      
      df_use <- subset(df_use1, ORIGIN_AIRPORT_NAME == input$origin_airport_sel,
                       select=c(AIRLINE, ARRIVAL_DELAY, DEPARTURE_TIME))
    }
    
    df_use <- df_use[which(df_use$AIRLINE %in% input$airline_sel), ]
    
    df_use <- df_use %>%
      filter(DEPARTURE_TIME >= input$timeofday[1], DEPARTURE_TIME <= input$timeofday[2])
    
    new_col_df <- as.data.frame(table(df_use$AIRLINE))
    airline.avg.delay <- 0
    
    keeps <- c("AIRLINE", "ARRIVAL_DELAY")
    df_use <- df_use[keeps]
    
    validate(
      need(nrow(df_use) > 0, "Please select other options, no data availabe for give arguments! Sorry")
    )
  
    airline.avg.delay <- aggregate(df_use$ARRIVAL_DELAY, by=list(df_use$AIRLINE), mean, na.rm=T)
    
    names(airline.avg.delay) <- c("AirlineCode", "Mean.Arrival.Delay")
    airline.avg.delay <- merge(airline.avg.delay, airline, by.x="AirlineCode", by.y="IATA_CODE", all.x=TRUE)
    
    airline.avg.delay <- airline.avg.delay[order(airline.avg.delay$Mean.Arrival.Delay), ]
    
    airline.avg.delay <- merge(airline.avg.delay, new_col_df, by.x="AirlineCode", by.y="Var1", all.x=TRUE, 
                               all.y = TRUE)

    return(airline.avg.delay)
    
  })
  
  output$plot  <- renderPlotly({

    ggplotly(ggplot(data=sub_flight()) +
     geom_vline(xintercept=0, linetype="dashed", color = "red", size=0.3)+
     geom_vline(xintercept = 15, linetype="dashed", color = "red", size=0.3)+
      geom_point(mapping = aes(x = Mean.Arrival.Delay, y = Freq, color=AIRLINE,
                               size = 1.5))+
        ylab('Number of Flights')+
       xlab('Average Arrival Delay')

      )
  })

#-------------------
# Taxi In @ Destination
#-------------------

taxinAirport<-reactive({
  
  df_use_2 <- subset(net_flights, DESTINATION_AIRPORT == input$airport_sel1,
                   select=c(TAXI_IN, AIRLINE_NAME))
  
  taxiIn<-subset(df_use_2,!is.na(df_use_2$TAXI_IN)) %>%
    group_by(AIRLINE_NAME) %>%
    dplyr::summarise(mean_taxiIn=mean(TAXI_IN), count_of_fly=n()) %>%
    arrange(AIRLINE_NAME)
  
  return(taxiIn)
  
})




output$plotTaxiIn  <- renderPlotly({
    ggplotly( 
     ggplot(taxinAirport(),aes(name = AIRLINE_NAME,x=mean_taxiIn,y=count_of_fly/100)) +
            geom_point(color='#FA6C00',alpha=0.4) +
            coord_cartesian(xlim=c(5,25)) +
            ylab('Count of flight(in 100s)') +
            xlab('Average Taxi In time') +
            geom_smooth(linetype=2, color="#AF0F00", aes(group=1), se=FALSE))

})




output$plotTaxiInAll <- renderPlotly({
  ggplotly(
    ggplot(taxiInA,aes(name = AIRLINE_NAME, x=mean_taxiIn,y=count_of_fly/100)) +
      geom_point(color='#FA6C00',alpha=0.4) +
      coord_cartesian(xlim=c(5,25)) +
      ylab('Count of flight(in 100s)') +
      xlab('Average Taxi In time') +
      geom_smooth(linetype=2, color="#AF0F00", aes(group=1), se=FALSE))

})

}


######################################################################################################


ui <- shinyUI(fluidPage(
  theme = shinytheme("united"),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = 'bootstrap.min.css')
  # ),
  navbarPage(
    "US flights delay analysis",
    tabPanel(
      "Landing Page",
      headerPanel(""),
      br(),
      h1(""),
      h1("US flights delay analysis", align = "center"),
      h2(""),
      h4("The air traffic system is bursting at the seams, with record numbers of passengers and flights clogging U.S. airports, creating bottlenecks on runways and in the air."),
      h2(''),
      h4('Flight delays cause lot of problems for airline customers in myriad ways and also result in financial loses for airline companies.'),
      h2(""),
      h4("The dataset provides information about the on-time performance (cancellations and delays) of domestic flights operated by large air carriers in the US. Aim is to analyse various types of delays affecting daily flight operation at airports in the US.", align = 'justify'),
      h2(""),
      h4("This analysis can help us narrow down to the places in urgent need of decongestion of traffic and people. It gives crucial insight about the places which need infrastructure development , additional runway construction, etc. "),
      h2(""),
      
      h5("Here's a fun GIF!"),
      withLoader(plotOutput("NOPLOTH"), type="image", loader="buffer.gif"),
      # the Image of report front page goes here!
    ####  # img(src = "report.png", height = 1200, width = 1300, align = 'center'),
      # the absolute panel is the widget that allows you to select by party
      h2(""),
      h2("")
    ),
    tabPanel(
      "Analysis",
      mainPanel(
        tabsetPanel(id = 'tabs1',
                    tabPanel("Map",
                             sidebarLayout(
                               
                               sidebarPanel(
                                 width = 2,
                                 h4('Map'),
                                 helpText("Airports in US")
                               ),
                               
                               mainPanel(
                                 width = 10,
                                 leafletOutput("map",
                                               width = "100%",
                                               height = "600px"),
                                 h1(''),
                                 h4("United States has arround 5000 public airports and 14000 private airports"),
                                 h4(''),
                                 h4("They handle about 44000 flight operation daily."),
                                 h4(''),
                                 h3("1 out of 3 flights are delayed at departure."),
                                 h4(''),
                                 h3("1 out of 4 flights depart 5 or more minutes earlier than scheduled.")
                                 )),

                    ),
                    
                    tabPanel("Taxi In",
                             h1(""),
                             sidebarLayout(
                               
                               sidebarPanel(
                                 width = 2,
                                 h4('Select relevent Indicators'),
                                 selectInput(
                                   "airport_sel1",
                                   "Port of Destinantion of Interest: ",
                                   choices = unique(sort(net_flights$DESTINATION_AIRPORT)),
                                   selected = 'PHL'
                                 ),
                                 hr(),
                                 helpText("A greater Taxi In time suggests, longer waiting queues, arrival gate being far from runway or
                                          fewer Gates at airport")
                               ),
                               
                               mainPanel(
                                 width = 10,
                                 h3(
                                   "Shows average Taxi In time for an Airport for different airlines"
                                 ),
                                 withLoader(plotlyOutput("plotTaxiIn"), type="html", loader="pacman"),
                                 
                                 h2(''),
                                 h3(
                                   "Shows average Taxi In time for all Airport for different airlines"
                                 ),
                                 withLoader(plotlyOutput("plotTaxiInAll"), type="html", loader="pacman")
                                 
                                 
                                 )),

                       ),
                    
                    tabPanel("Different delays",
                             sidebarLayout(
                               
                               sidebarPanel(
                                 width = 2,
                                 selectInput(
                                   "airport_sel2",
                                   "Port of Destinantion of Interest: ",
                                   choices = unique(sort(net_flights$DESTINATION_AIRPORT)),
                                   selected = 'PHL'
                                 ),
                                 hr()                               ),
                               
                               mainPanel(
                                 width = 10,
                                 withLoader(plotlyOutput("plot_delay_airlines"), type="html", loader="pacman"),
                                ))
                    )
        ))
    ),
    tabPanel(
      "Main Plot",
      headerPanel("Delay Plot"),
      h1(""),
      # the map is called here
      sidebarLayout(
        
        sidebarPanel(
          width = 3,
          selectInput(
            "dest_airport_sel",
            "Port of Destination of Interest: ",
            choices = unique(sort(net_flights$DESTINATION_AIRPORT_NAME)),
            selected = 'Miami International Airport'
          ),
          checkboxInput("fixed", "Remove port of Origin?"),
          conditionalPanel(condition = "input.fixed == 0",
                           selectInput(
                                 "origin_airport_sel",
                                 "Port of Origin of Interest: ",
                                 choices = unique(sort(net_flights$ORIGIN_AIRPORT_NAME)),
                                 selected = 'Seattle-Tacoma International Airport'
                               )),

          sliderInput("timeofday", "Time of Day",
                      min = 0000, max = 2359, value = c(1200, 1300)),
          
          checkboxGroupInput("day_week", "Select day/s :",
                             choices = c("Monday" = 1,   
                                         "Tuesday" = 2,   
                                         "Wednesday" = 3,   
                                         "Thursday" = 4,   
                                         "Friday" = 5,   
                                         "Saturday" = 6,   
                                         "Sunday" = 7),
                             selected = c(1,2,3,4,5,6,7)),
          
          checkboxGroupInput("airline_sel", "Airlines to display in graph:",
                             choices = c("United Air Lines Inc." = "UA",   
                                         "American Airlines Inc." = "AA",   
                                         "US Airways Inc." = "US",   
                                         "Frontier Airlines Inc." = "F9",   
                                         "JetBlue Airways" = "B6",   
                                         "Skywest Airlines Inc." = "OO",   
                                         "Alaska Airlines Inc." = "AS",   
                                         "Spirit Air Lines" = "NK",   
                                         "Southwest Airlines Co." = "WN",   
                                         "Delta Air Lines Inc." = "DL",   
                                         "Atlantic Southeast Airlines" = "EV",   
                                         "Hawaiian Airlines Inc." = "HA",   
                                         "American Eagle Airlines Inc." = "MQ",   
                                         "Virgin America" = "VX"),
                             selected = c("UA","AA", "US", "F9", "B6",
                                          "OO", "AS", "NK", "WN", "DL", "EV", "HA",
                                          "MQ", "VX")),
          

          hr(),
          h4('Select relevent Indicators')
        ),
        
        mainPanel(
          width = 9,
          withLoader(plotlyOutput('plot'), type="html", loader="pacman"),
          h4(
            strong("*** "),
            " The Average delay of 0 to 15 minutes is not considered as a Delayed Trip, i.e shown by two dotted red lines."
          ),
          h4(
            strong("1: "),
            "Choose an Airport to analyse."
          ),
          h4(
            strong("2: "),
            "Choose a range for time-of-day (24 hour scale)."
          ),
          h4(
            strong("3: "),
            "Select an Airline/ Airlines to Compare"
          )
        )
      )
      
      
    )

  )
))

shinyApp(ui = ui, server = server)
