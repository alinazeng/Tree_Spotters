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
d<-read.csv("individual_phenometrics_data.csv", header=TRUE)
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

names <- read.csv("ancillary_individual_plant_data.csv", header = TRUE)

# filter out unwanted columns
names <- dplyr::select(names, -c(4:19))
names <- dplyr::select(names, -Scientific_Name)
# separate plant nicknames into two columns
library(tidyr)
names <- names %>% 
  tidyr::separate(Plant_Nickname, c("Plant_ID", "Species_Nickname"), sep = ", ", 
                  remove = FALSE) 


# add coordinates



# duplicate genus column first

case_when(genus == "Fagus" ~ "Beech Route",
          genus == "Betula" ~ "Birch Route",
          genus == "Carya" ~ "Hickory Route"
          genus %in% c("Tilia", "Aesculus") ~ "Linden North Woods Route",
          genus == "Acer" ~ "Maple Route",
          genus == "Quercus" ~ "Oak Route",
          vector2 == "D" ~ "joob")

