library(rgdal)
library(plyr)
library(leaflet)
library(dplyr)
library(data.table)
library(DT)
library(car)
library(tidyverse)
library(sqldf)

# reading csv's
airport<-read.csv("airports.csv")
airline<-read.csv("airlines.csv")
flights<-read.csv("flights.csv") # takes 2-3 minutes

red_flight <- subset(flight, ORIGIN_AIRPORT == 'ORD' | ORIGIN_AIRPORT == 'PHL' ,
                     select=c(AIRLINE, ARRIVAL_DELAY, ORIGIN_AIRPORT))


flights<-subset(flights, !(is.na(flights$SCHEDULED_TIME)))
net_flights <- subset(flights, flights$CANCELLED==0)


net_flights <- subset(net_flights, !(is.na(net_flights$AIR_TIME)))
na_percentages <- sapply(net_flights, function(x) sum(is.na(x))) / nrow(net_flights) * 100
sort(format(round(na_percentages, 4), nsmall = 4))

imp_flight_data <- subset(flights, select = c(MONTH, DAY_OF_WEEK, AIRLINE, ORIGIN_AIRPORT,
                                              DESTINATION_AIRPORT, DEPARTURE_DELAY,SCHEDULED_TIME,
                                              ARRIVAL_DELAY, AIR_SYSTEM_DELAY, SECURITY_DELAY,
                                              AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY))

red_flight
# Manual filling codes for 3 airports
#######################
# manually pull missing coordinate values
#	Northwest Florida Beaches International Airport, Plattsburgh International Airport, 	Northeast Florida Regional Airport 
# LAT, LONG - (30.3548871,-85.8016595), (44.6520635,-73.470109), (29.9543946,-81.3450803)


airport[97, "LATITUDE"] <- 30.3548871
airport[97, "LONGITUDE"] <- -85.8016595
airport[235, "LATITUDE"] <- 44.6520635
airport[235, "LONGITUDE"] <- -73.470109
airport[314, "LATITUDE"] <- 29.9543946
airport[314, "LONGITUDE"] <- -81.3450803
  


# Airport Map USA front page


# US map of all Airports

## Plot
#########################  

library(leaflet.extras)
leaflet() %>% 
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


net_flights

new_obj <-net_flights %>% group_by(ORIGIN_AIRPORT_NAME, status) %>% tally()
new_obj
new_df_haha<-subset(net_flights,!is.na(df_use_2$status)) %>%
  group_by(ORIGIN_AIRPORT) %>%
  dplyr::summarise(mean_taxiIn=mean(TAXI_OUT), count_of_fly=n()) %>%
  arrange(ORIGIN_AIRPORT)

##########################

read.table("flights.csv", sep=",", header = TRUE, fill = TRUE, nrows = 5)

###########################################################
####### Making SQL Databases/Tables for flights csv########   # bad option 
###########################################################


{
  conemp <- dbConnect(SQLite(), dbname = "flights.db")
  if(dbExistsTable(conemp, "flights")) dbRemoveTable(conemp, "flights")
  
  dbWriteTable(conemp, "flights", 
               "flights.csv", sep = ",", header = TRUE,
               row.names = FALSE) #" RSQLite doesn't handle commas in quotes

  dbDisconnect(conemp)
}

conemp <- dbConnect(SQLite(), dbname = "flights.db")
res <- dbSendQuery(conemp, "
                    SELECT *
                    FROM flights
                    WHERE ARRIVAL_DELAY > 15
                    ")
fetch(res, n = 50)

# after fetching result, clear it
dbClearResult(res)



# order of IATA code according to number of datapoints of corr airline
################## 
unique(flight$AIRLINE)
sort(airline$IATA_CODE) == sort(unique(flight$AIRLINE))
airline
# ----------------------------------------------------------
# flights_count vs Mean.Arrival.delay
#################
list(table(red_flight$AIRLINE))

new_col_df <- as.data.frame(table(red_flight$AIRLINE))
new_col_df


airline.avg.delay <- 0
airline.avg.delay <- aggregate(red_flight$ARRIVAL_DELAY, by=list(red_flight$AIRLINE), mean, na.rm=T)
names(airline.avg.delay) <- c("AirlineCode", "Mean.Arrival.Delay")
airline.avg.delay <- merge(airline.avg.delay, airline, by.x="AirlineCode", by.y="IATA_CODE", all.x=TRUE)

airline.avg.delay <- airline.avg.delay[order(airline.avg.delay$Mean.Arrival.Delay), ]

airline.avg.delay <- merge(airline.avg.delay, new_col_df, by.x="AirlineCode", by.y="Var1", all.x=TRUE, 
                           all.y = TRUE)

airline.avg.delay

ggplot(data=airline.avg.delay) +
  geom_point(mapping = aes(x = Mean.Arrival.Delay, y = Freq, color=AIRLINE))

# add text to this

# airpot vs Mean.Arrival Delay for ORD - Chicago
###########################
list(table(flight$ORIGIN_AIRPORT))


# States with most airports
###############

table(airport$STATE)

top10 <- as.data.frame(table(airport$STATE))
top10 <- top10[order(top10$Freq, decreasing=T), ][1:10, ]
barplot(top10$Freq,
        names.arg = top10$Var1,
        col = "dark blue",
        main = "Number of Airports by State - Top 10",
        xlab = "Frequency",
        ylab = "State",
        horiz=T)


# Delay analysis by time(Month wise)
################################


flight$status<-'ON TIME'
flight$status[flight$DEPARTURE_DELAY>0] <-'DEPARTURE DELAY'
flight$status[flight$ARRIVAL_DELAY>0 & (flight$ARRIVAL_DELAY-flight$DEPARTURE_DELAY)>0 & flight$DEPARTURE_DELAY>0] <-'DEPR & ARVL_DELAY'
flight$status[flight$ARRIVAL_DELAY>0 & flight$DEPARTURE_DELAY<=0] <-'ARRIVAL_DELAY'
flight$status<-factor(flight$status)

ggplot(flight, aes(fill=status, x=DAY_OF_WEEK),binwidth=12) + 
  geom_bar(position="stack", stat="count") +
  scale_x_discrete("Day of the Week", breaks = c("1","2","3","4","5","6","7"), labels=c("Mon","Tues","Wed","Th","Fri","Sat","Sun"))



### Taxi time relation with num flights frm airport
###############################################
taxiInAirport<-subset(net_flights,!is.na(net_flights$TAXI_IN)) %>%
  group_by(DESTINATION_AIRPORT) %>%
  dplyr::summarise(mean_taxiIn=mean(TAXI_IN), count_of_fly=n()) %>%
  arrange(DESTINATION_AIRPORT)




taxiInAirport<-subset(net_flights,!is.na(net_flights$TAXI_IN)) %>%
  group_by(DESTINATION_AIRPORT,DESTINATION_CITY,DESTINATION_STATE) %>%
  dplyr::summarise(mean_taxiIn=mean(TAXI_IN), count_of_fly=n()) %>%
  arrange(DESTINATION_AIRPORT)
cor.test(taxiInAirport$mean_taxiIn,taxiInAirport$count_of_fly)


# install.packages("shinycustomloader")
# install.packages('shinydashboard')
library(shinycustomloader)
library(shinydashboard)

shinyExample()

withLoader(plotOutput("distPlot"), type="image", loader="buffer.gif")



marketShare <- flights %>% 
  group_by(AIRLINE) %>%
  dplyr::summarise(Count = n(),
                   mean_ARRIVAL_DELAY = mean(ARRIVAL_DELAY),
                   median_ARRIVAL_DELAY = median(ARRIVAL_DELAY),
                   min_ARRIVAL_DELAY = min(ARRIVAL_DELAY),
                   max_ARRIVAL_DELAY = max(ARRIVAL_DELAY),
                   mean_DEPARTURE_DELAY = mean(DEPARTURE_DELAY),
                   median_DEPARTURE_DELAY = median(DEPARTURE_DELAY),
                   min_DEPARTURE_DELAY = min(DEPARTURE_DELAY),
                   max_DEPARTURE_DELAY = max(DEPARTURE_DELAY)
  ) %>% arrange(desc(Count))


plot_ly(marketShare, labels = ~AIRLINE, values = ~Count, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        marker = list(
          line = list(color = '#FFFFFF', width = 1)),
        showlegend = FALSE)
net_flights

net_flights$status<-'ON TIME'
net_flights$status[net_flights$DEPARTURE_DELAY>0] <-'DEPARTURE DELAY'
net_flights$status[net_flights$CANCELLED==1] <-'CANCELLED FLIGHTS'
net_flights$status[net_flights$ARRIVAL_DELAY>0 & (net_flights$ARRIVAL_DELAY-net_flights$DEPARTURE_DELAY)>0 & net_flights$DEPARTURE_DELAY>0] <-'DEPR & ARVL_DELAY'
net_flights$status[net_flights$ARRIVAL_DELAY>0 & net_flights$DEPARTURE_DELAY<=0] <-'ARRIVAL_DELAY'
net_flights$status<-factor(net_flights$status)

ggplotly(ggplot(net_flights,aes(x=AIRLINE,fill=status),binwidth=12) + 
  geom_bar(position = "fill") + 
  ggtitle("Flights by Months According to Flights Status"))



net_flights$DEP_HOUR <- NA
net_flights$DEP_HOUR <- as.integer(net_flights$DEPARTURE_TIME / 100)

net_flights$modified_delay <- NA
net_flights$modified_delay <- case_when(
  net_flights$ARRIVAL_DELAY <= 5 ~"On Time (0-5 mins)",
  net_flights$ARRIVAL_DELAY > 5 & net_flights$ARRIVAL_DELAY <= 15 ~"Small Delay (5-15 mins)",
  net_flights$ARRIVAL_DELAY > 15 ~ "Large Delay (> 15 mins)" )


ggplot(data=net_flights, aes(x=factor(net_flights$DAY_OF_WEEK),fill=factor(net_flights$modified_delay))) + 
  geom_histogram(stat = "Count",position = "dodge", col="red") +
  labs(title="Histogram for Airline") +
  labs(x="Departure Hours", y="Number of Flights") + 
  scale_x_discrete() +
  labs(x = c("Monday", "Tuesday",
                  "Wednesday", "Thursday",
                  "Friday", "Saturday",
                  "Sunday"))

