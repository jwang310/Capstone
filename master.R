library(ggmap)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(shiny)
library(lubridate)
library(data.table)
library(dplyr)


#set wd, load and fortify traffic data
setwd("C:/Users/camyb/Documents/Capstone/Data")

data <- read.csv("Master Incident List (1).csv", stringsAsFactors = FALSE)
data_central <- data_central[(data_central$Region == "Central Region"), ]

#make lon and lat more easily identifiable
colnames(data_central)[which(colnames(data_central) == "Start.Longitude.Nbr")] <- "Longitude"
colnames(data_central)[which(colnames(data_central) == "Start.Latitude.Nbr")] <- "Latitude"

#change type of data from character to date-time
data_central$Created.Dt <- parse_date_time(data_central$Created.Dt, orders = "mdy %H:%M:%S")
data_central$Cleared.Dt <- parse_date_time(data_central$Cleared.Dt, orders = "mdy %H:%M:%S")
data_central$Sceneclear.Dt <- parse_date_time(data_central$Sceneclear.Dt, orders = "mdy %H:%M:%S")
data_central$Closed.Dt <- parse_date_time(data_central$Closed.Dt, orders = "mdy %H:%M:%S")

#extract only the time from the date-time object and convert to POSIXct once again
data_central$CreatedHours <- format(as.POSIXct(strptime(data_central$Created.Dt,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
data_central$CreatedHours <- parse_date_time(data_central$CreatedHours, orders = "%H:%M:%S")

data_central$ClearedHours <- format(as.POSIXct(strptime(data_central$Cleared.Dt,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
data_central$ClearedHours <- parse_date_time(data_central$ClearedHours, orders = "%H:%M:%S")

data_central$SceneclearHours <- format(as.POSIXct(strptime(data_central$Sceneclear.Dt,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
data_central$SceneclearHours <- parse_date_time(data_central$SceneclearHours, orders = "%H:%M:%S")

data_central$ClosedHours <- format(as.POSIXct(strptime(data_central$Closed.Dt,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
data_central$ClosedHours <- parse_date_time(data_central$ClosedHours, orders = "%H:%M:%S")

#separate CreatedHours into increments of 30 to better see where accidents are occurring
data_central$CreatedIncrements = cut(data_central$CreatedHours, breaks = "60 min")


#create dataframe grouping based off of time of day, add in weekdays
data_by_time <- data_central%>% group_by(data_central$CreatedIncrements)

data_by_time$day <- weekdays(as.Date(data_by_time$Created.Dt))


#limit to only interstate and primary roadways
data_by_time <- data_by_time[(data_by_time$Segment.Type == c("Interstate", "Primary")), ]

# set to chunks of every three milemarkers
data_by_time$Milemarker.Nbr <- cut_width(data_by_time$Milemarker.Nbr,width = 3, boundary = 0)


#data table-- just counts of segment types
data_by_segment_type <- data_central %>%
  count(#Milemarker.Nbr,
    # day,
    # CreatedIncrements,
    Segment.Type)

#data table-- counts of accidents given day, time, route, and milemarker
data_by_milemarker <- data_by_time %>%
  count(Milemarker.Nbr,
        day,
        CreatedIncrements,
        Route.Nm)

#find the probabilities and add back appropriate columns
data_probabilities <- as.data.table(data_by_milemarker)[ , list( prob = n/sum(n) ), by = list(day, CreatedIncrements)  ]
data_probabilities$Milemarker.Nbr <- data_by_milemarker$Milemarker.Nbr
data_probabilities$Route.Nm <- data_by_milemarker$Route.Nm
data_probabilities

prob_final <- data_probabilities[with(data_probabilities, order(day, CreatedIncrements, -prob)),]
