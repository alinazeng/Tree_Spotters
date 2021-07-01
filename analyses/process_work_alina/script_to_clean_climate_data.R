# script to clean climate data of Boston
# alina.zeng(at)ubc.ca
# May-28, 2021

# data downloaded from https://labs.arboretum.harvard.edu/weather/


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# install.packages("weathermetrics")
# load Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(weathermetrics)



# Set Working Directory
setwd("C:/Users/alina/Documents/git/Tree_Spotters")

# import data
data <- read.csv("input/weldhill.csv", header = T)

# getting rid of years we don't need
data <- data[-c(1:102398),]

# keeping only the columns we are interested in 
data <- select(data,c(Eastern.daylight.time, Temp..F, Temp.Soil..F, Dewpt..F))

# remaming columns
data <- rename(data, EDT = Eastern.daylight.time, 
               temp_F = Temp..F, temp_soil_F =Temp.Soil..F, dew_point_F = Dewpt..F)

# separate EDT into two columns
data <- data %>% 
  tidyr::separate(EDT, c("mmddyyyy", "time"), sep = " ", 
                  remove = FALSE) 

data$yyyy_mm_dd <- as.Date(data$mmddyyyy,"%m/%d/%Y")


# convert time to year, month, date
data <- mutate (data,
                year = as.numeric(format(data$yyyy_mm_dd, format = "%Y")),
                month = as.numeric(format(data$yyyy_mm_dd, format = "%m")),
                day = as.numeric(format(data$yyyy_mm_dd, format = "%d")))


# calculate daily temperature

summary_daily_temp <- data %>% 
  group_by(yyyy_mm_dd) %>% 
  summarise("daily_temp_F" = mean(temp_F))
            
summary_daily_soil_temp <- data %>% 
  group_by(yyyy_mm_dd) %>% 
  summarise("daily_temp_soil_F" = mean(temp_soil_F))           

summary_daily_dew_pt <- data %>% 
  group_by(yyyy_mm_dd) %>% 
  summarise("daily_dew_point_F" = mean(dew_point_F))     


# join data frames 

data <- full_join(data,summary_daily_dew_pt)
data <- full_join(data,summary_daily_soil_temp)
data <- full_join(data,summary_daily_temp)
                 
# create a column for DOY
data <- data %>% 
  mutate("doy"=yday(yyyy_mm_dd))

# convert temperature to Celsius

data$daily_temp_C <- fahrenheit.to.celsius(data$daily_temp_F)
data$daily_temp_soil_C <- fahrenheit.to.celsius(data$daily_temp_soil_F)
data$daily_dew_point_C <- fahrenheit.to.celsius(data$daily_dew_point_F)

# keep only the columns we will use and condense the table

data <- data %>% 
  select(c(yyyy_mm_dd,year,month,day,doy,
           daily_temp_C,daily_temp_soil_C,daily_dew_point_C, daily_temp_F,
           daily_temp_soil_F,daily_dew_point_F)) %>% 
  unique()

# round up digits
data <- data %>% mutate_if(is.numeric, ~round(., 2))  

# export to csv

write.csv(data, file = "output/cleaned_climate_data_year_2015_2020.csv", row.names = F)