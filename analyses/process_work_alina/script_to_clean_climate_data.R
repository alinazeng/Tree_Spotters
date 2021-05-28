# script to clean climate data of Boston
# alina.zeng(at)ubc.ca
# May-28, 2021

# data downloaded from https://labs.arboretum.harvard.edu/weather/


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load Libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Set Working Directory
setwd("C:/Users/alina/Documents/git/Tree_Spotters")

# import data
data <- read.csv("input/weldhill.csv", header = T)

# getting rid of years we don't need
data <- data[-c(1:102398),]

# keeping only the columns we are interested in 
data <- select(data,c(Eastern.daylight.time, Temp..F, Temp.Soil..F, Dewpt..F))

# remaming columns
data <- rename(data,EDT = Eastern.daylight.time, 
               temp_F = Temp..F, temp_soil_F =Temp.Soil..F, dew_point_F = Dewpt..F)

# separate EDT into two columns
data <- data %>% 
  tidyr::separate(EDT, c("mmddyyyy", "time"), sep = " ", 
                  remove = FALSE) 

data$yyyy_mm_dd <- as.Date(data$mmddyyyy,"%m/%d/%Y")

data <- mutate (data,
                year = as.numeric(format(data$yyyy_mm_dd, format = "%Y")),
                month = as.numeric(format(data$yyyy_mm_dd, format = "%m")),
                day = as.numeric(format(data$yyyy_mm_dd, format = "%d")))


data <- data %>% 
  mutate (date = data$mmddyyyy,
                year = as.numeric(format(data$mmddyyyy, format = "%Y")),
                month = as.numeric(format(data$mmddyyyy, format = "%m")),
                day = as.numeric(format(data$mmddyyyy, format = "%d")))


data <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))


# convert time to year, month, date


DOY


average temp



)

                 

