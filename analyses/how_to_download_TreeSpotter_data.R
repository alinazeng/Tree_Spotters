# Hello Tree Spotters & fellow enthusiasts!
# This script was created to provide you some guidance on 
   # downloading Tree Spotter data
   # tidying up the data so you can perform some simple analysis as you wish

# Note that this script has the exact same content as our lovely wiki page here 
# at https://github.com/alinazeng/Tree_Spotters/wiki/How-to-download-tidy-analyze-Tree-Spotters-data
# If you prefer looking at the wiki page, by all means~

# Contact ----
# alinazengziyun@yahoo.com

# Instructions on downloading data ----

# 1) Go to the NPN Data Downloader Tool at: https://data.usanpn.org/observations
#    Go to the Phenology Observation Portal
# 2) Select 'Individual Phenometrics' and press NEXT 
# 3) Set Date range applicable to your question and press 'Set Date' and NEXT
# 4) Select 'Partner Groups' tab on the left: press the + next to 
#    'Botanic Gardens and Arboretums and select 'Arnold Arboretum - Tree Spotters'
#    Press 'Set Groups' and NEXT
# 5) Select 'Output fields' tab on the left and select fields that you are interested in
#    Press 'Set Output Fields' then click the box next to 'I have read and 
#    acknowledged...' on the right and then click 'Download'
# 6) Go to your Downloads and unzip the datasheet_XX.zip. 
#    Move the `individual_phenometrics_data.csv' to /a_folder_of_your_choice/


# if you would like to clean up the data a bit, please follow the steps below ----

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(lubridate)

# Set Working Directory ----
setwd("C:/Users/your_name/Documents/git/Tree_Spotters")  # path to where you saved the data


# Import Tree Spotters data and clean :) ----
# Note that you can find a csv file of individual_phenometrics data across 
# Year 2016-2021 in this repository: Tree_Spotters/input/individual_phenometrics_data_all_columns.csv  (https://github.com/alinazeng/Tree_Spotters/blob/main/input/individual_phenometrics_data_all_columns.csv)
# You can view the description of column names here: Tree_Spotters/input/individual_phenometrics_datafield_descriptions.xlsx

raw <- read.csv("input/status_intensity_observation_data_all_columns.csv", header = TRUE)
# importing the csv file and calling it "raw" for simplicity

# select only columns we need
subraw<-subset(raw, select= c( "Observation_ID",  "ObservedBy_Person_ID",  "Observation_Date" ,
                                     "Site_Name", "Genus","Species" , "Common_Name"  , "Individual_ID", 
                                     "Phenophase_Description" ,   "Day_of_Year" , "Phenophase_Status", "year"))	

# rename to make column names more digestable
subraw <- rename(subraw,observation_id = Observation_ID,treespotter_id = ObservedBy_Person_ID,
                 observation_date = Observation_Date, route = Site_Name, genus = Genus, 
                 species = Species,common_name = Common_Name,tree_id = Individual_ID, 
                 phase = Phenophase_Description,   
                 doy = Day_of_Year, status = Phenophase_Status)

subraw$phase<-ifelse(subraw$phase=="Breaking leaf buds", "budburst", subraw$phase)   # this modifies the phenophase descriptions
subraw$phase<-ifelse(subraw$phase=="Leaves", "leafout", subraw$phase)
subraw$phase<-ifelse(subraw$phase=="Flowers or flower buds", "flowers", subraw$phase)
subraw$phase<-ifelse(subraw$phase=="Falling leaves", "leaf drop", subraw$phase)

# let us tidy up citizen science data 
# note: everyting in this script suggests what you can do, not necessarily what 
# you have to do; please feel free to use commands that are applicable to your interest


# the logic behind my cleaning method here is to accept all observations that either 
# have multiple observers or 
# happen within a 5-day frame of other closeby observations

subraw <- subset(subraw,subraw$status != "-1" & subraw$status != "0") # we are interested in the "yes" observations 
# (refer to document page X for what yes and no obserbations are)

# order doy based on year, phase, tree_id
subraw <- subraw   %>% 
  group_by(year, phase, tree_id) %>%
  arrange(-doy, .by_group = TRUE)      # descending
# calculate the day difference
subraw <- subraw  %>% group_by(year, phase, tree_id)%>%
  mutate(difference_reverse = lag(doy,default=first(doy))-doy)


# reverse the ordering and calculate the difference again
subraw <- subraw   %>% 
  group_by(year, phase, tree_id) %>%
  arrange(doy, .by_group = TRUE)    # ascending

subraw <- subraw  %>% group_by(year, phase, tree_id)%>%
  mutate(difference = doy - lag(doy,default=first(doy)))

# add number of observations
subraw <- subraw  %>% group_by(year, phase, tree_id, doy) %>% 
  mutate(obs_num = sum(status))          

# filter out extremes
clean <- filter(subraw,difference < 5 |obs_num > 1)  
clean <- clean[!(clean$difference == 0 & clean$difference_reverse >5 & clean$obs_num==1),]  
# this gets rid of first doys observed by a single observer that are too far from the rest of observations 

# quickly calcultate max and min and range
summ_clean <- clean %>% group_by(common_name,year, phase, tree_id) %>% 
  summarise(doy_mean=mean(doy), doy_median = median(doy),obs_number=length(doy),first_doy = min(doy),
            last_doy = max(doy), maximn_range = max(doy)-min(doy), interquartile_range = IQR(doy))
						
# you can view d to make sure nothing weird is going on ----
view(subraw)
str(subraw)
dim(subraw)
summary(subraw) # a few ways to get ahold of the attributes and characteristics 


