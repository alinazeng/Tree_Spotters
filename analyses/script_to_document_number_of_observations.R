# This script will be used to figure out which routes/species/individuals were most frequently observed
# Development is underway
# May-13-2021
# alinazeng(at)ubc.ca


# can represent some info using pie charts 
# eg. out of 300000 records, 40% (120000 were made on XXX route)
# http://data.usanpn.org/npn-viz-tool/


# also need to work on mapping using GIS
# need to download coordinates from this site: https://arboretum.harvard.edu/explorer/
# clean/format csv. files and then map
# http://map.arboretum.harvard.edu/arcgis/rest/services/CollectionExplorer/MapServer


# hmmmmm documenting my to-do-list for Monday, May 17
# calculate observations -> by individual, spp, route, across phenophases
# download coordinates data and compile everything together
# meet cat <333


# Update on May-1-2021

# libraries ----
library(dplyr)
library(tidyr)
library(lubridate)

# set working directory ----
setwd("C:/Users/alina/Documents/git/Tree_Spotters")


# Import TreeSpotters data and clean :) ----
d<-read.csv("input/individual_phenometrics_data.csv", header=TRUE)
# importing the csv file and calling it "d" for simplicity

# tidy up citizen science data real quick

d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] 
# This selects data where multiple people observed the same phenophase
d <- d[(d$NumYs_in_Series>=3),] 
# This selects data where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] 
# This limits to data where a no is followed by a yes, so that it is a new
# observation/new phenophase but has been detected within a fair timeframe
# 4156 observations

# keep columns that we will use ----
d <- dplyr::select(d, c(Species_ID,Genus, Species, Common_Name, Individual_ID, 
                        Phenophase_ID, Phenophase_Description,First_Yes_DOY))

# join the genus and the species columns ----
d$Scientific_Names<- with(d, paste(Genus, Species, sep = " "))


# count the number of observations made on individual trees and shrubs
indiv_obs  <- d %>%
  group_by(Individual_ID) %>%
  summarise("indiv_obs#" = length(Individual_ID))

# count the number of observations made on each species
spp_obs <- d %>%
  group_by(Common_Name) %>%
  summarise("spp_obs#" = length(Common_Name))

# make a table with species name, coordinates, # of observation, individual ID
d <- full_join(d,indiv_obs)
d <- full_join(d,spp_obs)

# combine two systems of naming

names <- read.csv("input/ancillary_individual_plant_data.csv", header = TRUE)

# filter out unwanted columns
names <- dplyr::select(names, -c(4:19))
names <- dplyr::select(names, -Scientific_Name)
# separate plant nicknames into two columns
library(tidyr)
names <- names %>% 
  tidyr::separate(Plant_Nickname, c("Plant_ID", "Species_Nickname"), sep = ", ", 
                  remove = FALSE) 

# join tables
d <- full_join(d, names)

# add coordinates
# downloaded data from https://arboretum.harvard.edu/explorer/
# import data

treeinfo <- read.csv("input/MyVisit_filtered.csv", header=TRUE)

# rename columns and filter out unwanted ones
treeinfo <- rename(treeinfo,Plant_ID = Plant.ID,
                   Latitude = Garden.Latitude,
                   Longitude = Garden.Longitude,
                   "DBH(cm)" = DBH)

# joining data frames by Plant_ID
d_with_coordinates <- full_join(d, treeinfo)


# figure out which ones are not included in the routes ----
test5 <- full_join(names, treeinfo)
# 22798*A
# 14585*B
# 86273
# 86275
# 86277 :')))))

# duplicate Genus column first

case_when(Genus == "Fagus" ~ "Beech Route",
          Genus == "Betula" ~ "Birch Route",
          Genus == "Carya" ~ "Hickory Route"
          Genus %in% c("Tilia", "Aesculus") ~ "Linden North Woods Route",
          Genus == "Acer" ~ "Maple Route",
          Genus == "Quercus" ~ "Oak Route",
          Genus %in% c("Vaccinium","Viburnum","Hamamelis") ~ "Shrub Route")
# Important: need to use Plant_ID to reassign Peters Hill Route
Plant_ID %in% c()~ "Peters Hill Route"

d_with_coordinates <- d_with_coordinates %>%   # overwriting our data frame 
  mutate(Route_Name =   # creating our new column
           case_when(Genus == "Fagus" ~ "Beech Route",
                     Genus == "Betula" ~ "Birch Route",
                     Genus == "Carya" ~ "Hickory Route",
                     Genus %in% c("Tilia", "Aesculus") ~ "Linden North Woods Route",
                     Genus == "Acer" ~ "Maple Route",
                     Genus == "Quercus" ~ "Oak Route",
                     Genus %in% c("Vaccinium","Viburnum","Hamamelis") ~ "Shrub Route")
        )

# Important: need to use Plant_ID to reassign Peters Hill Route
d_with_coordinates[d_with_coordinates$Plant_ID %in% c("1323-82*A","16611*F","16611*J","16611*K",
                                                      "16611*O","689-2010*A","611-2010*A","22099*A","12651*I","17538*A",
                                                      "1104-81*A"), ]$Route_Name <- "Peters Hill Route"  



# count the number of observations made along each route
route_obs <- d_with_coordinates %>%
  group_by(Route_Name) %>%
  summarise("route_obs#" = length(Route_Name))

# make a table with species name, coordinates, # of observation, individual ID
d_with_coordinates <- full_join(d_with_coordinates,route_obs)
