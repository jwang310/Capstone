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


#create dataframe grouping based off of time of day
data_by_time <- data_central%>% group_by(data_central$CreatedIncrements)

data_by_time$day <- weekdays(as.Date(data_by_time$Created.Dt))

data_by_time <- data_by_time[(data_by_time$Segment.Type == c("Interstate", "Primary")), ]

# 
# 
# 
# #create a data frame based on weekdays 
# weekDays <- unique(data_by_time$day)
# 
# central_byDay <- lapply(1:7, function(x) data_by_time[which(data_by_time$day==weekDays[x]), ])
# names(central_byDay) <- weekDays
# 
# #now take that and expand the data base to include elements based on the time the incident occurred
# timeBreaks <- sort(unique(data_by_time$CreatedIncrements))
# timeBreaks
# central_byDayTime <- central_byDay
# 
# y=1
# for (i in 1:7){
#   print(y)
#   central_byDayTime[[i]] <- lapply(1:48, function(x) central_byDayTime[[i]][which(central_byDayTime[[i]]$CreatedIncrements==timeBreaks[x]),])
#   y=y+1
# }
# x <- central_byDayTime[[3]][[3]]
# 
# #now do it by road
# roadBreaks <- sort(unique(data_by_time$Route.Nm))
# roadBreaks
# central_byDayTimeRoad <- central_byDayTime
# 
# y=1
# for (i in 1:7){
#   for(j in 1:48){
#   print(y)
#   central_byDayTimeRoad[[i]][[j]] <- lapply(1:197, function(x) central_byDayTimeRoad[[i]][[j]][which(central_byDayTimeRoad[[i]][[j]]$Route.Nm==roadBreaks[x]),])
#   y=y+1
# }
# }
# 
# 
# x <- central_byDayTimeRoad[[3]][[12]][[which(roadBreaks == "I-95S")]]
# 

str(data_by_time)
data_by_time$Milemarker.Nbr <- cut_width(data_by_time$Milemarker.Nbr,width = 3, boundary = 0)

data_by_segment_type <- data_central %>%
  count(#Milemarker.Nbr,
       # day,
       # CreatedIncrements,
        Segment.Type)

data_mon <- data_by_time %>%
  count(Milemarker.Nbr,
        day = "Monday",
        CreatedIncrements)
data_mon$prob <- data_mon$n/sum(data_mon$n)
data_mon

# hi
