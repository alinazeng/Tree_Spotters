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

# import data ----
temp <- read.csv("output/cleaned_climate_data_year_2015_2020.csv", header = T)

# rename columns
temp2 <- rename(temp,
               "Temperature" = daily_temp_C,
               "Soil Temperature" = daily_temp_soil_C,
               "Dew Point" = daily_dew_point_C)
             
# subset
temp2015 <- temp2[temp2$year == 2015,]

# hmmm retaining only date and temp
temp2015 <- temp2015 %>% select(c(doy, Temperature))

temp2015<- temp2015 %>% select(c(doy, "Temperature", "Soil Temperature", "Dew Point"))


# Using Dygraph package for fun
dygraph(temp2015, main = "Arnold Arboretum Daily Temperature, 2015", 
        xlab = "Day of Year", ylab = "Temperature (\u00B0C)" )
# too bad Idk how to export this kind of interactive plots other than to html
# at the moment I only know to take a screenshot of the html, which isnt helpful
webshot("file:///C:/Users/alina/Downloads/temp2015_dygraph.html", "lala.png")


png(filename="Temp_complete_2015.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(temp2015, aes(x = doy, y = Temperature)) +  ### will need to see how to plot multiple lines at once
  geom_line(color = 6,    
            lwd = 0.8,      
            linetype = 1)+
  geom_smooth()+ 
  theme_classic()+
  labs(title = "Arnold Arboretum Daily Temperature, 2015", 
       subtitle = "Boston,Massachusetts",
       caption = "Data from Arnold Arboretum Weather Database", 
       x = "\n Day of Year", y = "Temperature (\u00B0C) \n")     # \n adds space before x and after y axis text
dev.off()



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


# could try ploting all the years tgt in one go
# need to look the code up
