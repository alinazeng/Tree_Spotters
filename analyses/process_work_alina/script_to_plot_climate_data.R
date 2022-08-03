# script to plot climate data
# May-28, 2021

source("analyses/process_work_alina/script_to_clean_climate_data.R")

# libraries ----
install.packages("dygraphs")
install.packages("Cairo")
install.packages('webshot') 
webshot::install_phantomjs()
library(dygraphs)
library(Cairo)
library(webshot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# import data ----
temp <- read.csv("output/cleaned_climate_data_year_2015_2020.csv", header = T)

# rename columns
temp <- rename(temp,
               "Temperature" = daily_temp_C,
               "Soil Temperature" = daily_temp_soil_C,
               "Dew Point" = daily_dew_point_C)
             
# get rid of 2015
temp <- temp[temp$year != 2015,]

# hmmm retaining only date and temp
temp <- temp %>% select(c(year,doy, Temperature))

temp2015<- temp2015 %>% select(c(doy, "Temperature", "Soil Temperature", "Dew Point"))


# Using Dygraph package for fun
dygraph(temp2015, main = "Arnold Arboretum Daily Temperature, 2015", 
        xlab = "Day of Year", ylab = "Temperature (\u00B0C)" )
# too bad Idk how to export this kind of interactive plots other than to html
# at the moment I only know to take a screenshot of the html, which isnt helpful
webshot("file:///C:/Users/alina/Downloads/temp2015_dygraph.html", "lala.png")


temp$year <- as.factor(temp$year)

png(filename="Temp_complete.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(temp[temp$year==2016,], aes(x = doy, y = Temperature), fill = year) +  ### will need to see how to plot multiple lines at once
  # geom_line(color = 6, lwd = 0.8, linetype = 1)+
  geom_line(aes(color = year)) +
  scale_x_continuous(limits = c(100, 140))+
  theme_classic()+
  labs(x = "\n Day of Year", y = "Temperature (\u00B0C) \n")     # \n adds space before x and after y axis text
dev.off()


# hmmm facet
ggplot(temp, aes(x = doy, y = Temperature), color = year) +  ### will need to see how to plot multiple lines at once
  # geom_line(color = 6, lwd = 0.8, linetype = 1)+
  geom_line(aes(color = year)) +
  scale_x_continuous(limits = c(100, 140),breaks=seq(100,140,5))+
  theme_classic()+
  facet_wrap(~ year, scales = "fixed") +  
  scale_y_continuous(limits = c(-5,30), breaks=seq(-5,30,5))+
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1))+
  labs(x = "\n Day of Year", y = "Temperature (\u00B0C) \n")
       
ggsave(file="temp_year_facet.svg", width=10, height=8)

ggplot(temp, aes(x = doy, y = Temperature), color = year) +  ### will need to see how to plot multiple lines at once
  # geom_line(color = 6, lwd = 0.8, linetype = 1)+
  geom_line(aes(color = year)) +
  scale_x_continuous(limits = c(100, 140),breaks=seq(100,140,5))+
  theme_classic()+
  facet_grid(~ year, scales = "fixed") +  
  scale_y_continuous(limits = c(0,30), breaks=seq(-5,30,5))+
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1))+
  labs(x = "\n Day of Year", y = "Temperature (\u00B0C) \n")
ggsave(file="temp_year_facet_grid.svg", width=10, height=6)

# non facet
ggplot(temp, aes(x = doy, y = Temperature), color = year) +  ### will need to see how to plot multiple lines at once
  # geom_line(color = 6, lwd = 0.8, linetype = 1)+
  geom_line(aes(color = year)) +
  scale_x_continuous(limits = c(100, 140),breaks=seq(100,140,5))+
  theme_classic()+
  scale_y_continuous(limits = c(-5,30), breaks=seq(-5,30,5))+
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1))+
  labs(x = "\n Day of Year", y = "Temperature (\u00B0C) \n")

ggsave(file="temp_year.svg", width=10, height=8)


# try out only plotting until doy = 140

png(filename="Temp_140_2015.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(temp2015, aes(x = doy, y = Temperature)) +  ### will need to see how to plot multiple lines at once
  geom_line(color = 6,    
            lwd = 0.8,      
            linetype = 1)+
  scale_x_continuous(limits = c(0, 140))+
  geom_smooth()+ 
  theme_classic()+
  labs(title = "Arnold Arboretum Daily Temperature, 2015", 
       subtitle = "Boston,Massachusetts",
       caption = "Data from Arnold Arboretum Weather Database", 
       x = "\n Day of Year", y = "Temperature (\u00B0C) \n")     # \n adds space before x and after y axis text
dev.off()


# could try ploting all the years tgt in one big plot
# need to look the code up
