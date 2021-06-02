# script to organize Status and Intensity data
# June 2
# alina.zeng(at)ubc.ca

# hmmm update on June-2, just realized I should use Status and Intensity data 
# rather than individual phenometrics data to document the frequence of observation


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)


# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/Tree_Spotters")

# update on June 2
d <- read.csv("input/status_intensity_observation_data.csv", header = TRUE)
# note that this document is huge... I will not be pushing it to github therefore
# 334180 obs


# rename the columns to make them more digestible ----
bb <- rename(d, phase=Phenophase_Description, 
             id=Individual_ID, genus=Genus, species=Species, 
             observerID = ObservedBy_Person_ID, route = Site_Name)

# make a column for year
bb$Observation_Date <- as.Date(bb$Observation_Date)

bb <- mutate (bb,
                year = as.numeric(format(bb$Observation_Date, format = "%Y")),
                month = as.numeric(format(bb$Observation_Date, format = "%m")),
                day = as.numeric(format(bb$Observation_Date, format = "%d")))

## subset and adjust the names of the phases
bb.pheno <- dplyr::select(bb, genus, species, Common_Name, phase, 
                          year, id, route, observerID, Plant_Nickname, Phenophase_Status)
## if bb.pheno$phase=="Breaking leaf buds", change it to "budburst", otherwise keep it as it is
## the same goes for the codes below
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)


# figure out which ones are valid, which ones are not
# 1 ["yes the phenophase is occuring"]|0 ["no the phenophase is not occuring"]
# |-1 ["not certain whether the phenophase is occuring"]
occurence <- as.data.frame(table(unlist(bb.pheno$Phenophase_Status)))

occurence <- occurence %>% mutate(status = 
                        case_when(Var1 == "1" ~ "Yes, the phenophase is occuring",
                                  Var1 == "0" ~ "No, the phenophase is not occuring",
                                  Var1 == "-1" ~ "Unsure"))

# rename columns
occurence <- rename(occurence, "frequency" = "Freq", "indicator" = "Var1")

# number of observations across years 
observation_year<- as.data.frame(table(unlist(bb.pheno$year)))
observation_year <- rename(observation_year, "frequency" = "Freq", "year" = "Var1")

# see how many volunteers have participated, and their contribution
length(unique(bb.pheno$observerID))
observer <- as.data.frame(table(unlist(bb.pheno$observerID)))
observer <- rename(observer, "frequency" = "Freq", "observerID" = "Var1")
# 226 different observers have contributed 
# as high as 30291 Yes/Nos to as low as 3 entered to data portal

# changes in number of observations by individual over years

observer_year<- bb.pheno %>% 
  group_by(observerID, year) %>% summarize(observation=length((observerID)))

observer_year$observerID <- as.factor(observer_year$observerID)

# join tables and rename
observer_year <- full_join(observer_year,observer)
observer_year <- rename(observer_year, observation_year = observation,
                        observation_total = frequency)


# export 
write.csv(occurence, file = "output/occurence_of_observation_status_June_2.csv", row.names = F)
write.csv(observation_year, file = "output/number_of_observation_each_year_June_2.csv", row.names = F)
write.csv(observer_year, file = "output/number_of_observation_by_each_individual_each_year_June_2.csv", row.names = F)


# get rid of Os and -1s and recalculate # of observations

pheno_cleaned <- subset(bb.pheno,bb.pheno$Phenophase_Status == "1")
# 66963 obs

# use this to calculate numbers of observation ----
# combine two systems of naming


# separate plant nicknames into two columns
library(tidyr)
pheno_cleaned <- pheno_cleaned %>% 
  tidyr::separate(Plant_Nickname, c("Plant_ID", "Species_Nickname"), sep = ", ", 
                  remove = FALSE) 

treeinfo <- read.csv("input/MyVisit_filtered.csv", header = TRUE)

# rename columns and filter out unwanted ones
treeinfo <- rename(treeinfo,Plant_ID = Plant.ID,
                   Latitude = Garden.Latitude,
                   Longitude = Garden.Longitude,
                   "DBH(cm)" = DBH)

# joining data frames by Plant_ID
d <- full_join(pheno_cleaned, treeinfo)



# count the number of observations made along each route ----
route_obs <- d %>%
  group_by(route) %>%
  summarise("route_obs#" = length(route))

# make a table with species name, coordinates, # of observation, individual ID
d <- full_join(d,route_obs)

# count the number of observations made on individual trees and shrubs ----
indiv_obs  <- d %>%
  group_by(id) %>%
  summarise("indiv_obs#" = length(id))

# count the number of observations made on each species
spp_obs <- d %>%
  group_by(Common_Name) %>%
  summarise("spp_obs#" = length(Common_Name))

# count the number of observations made on each phenophase
pheno_obs <- d %>%
  group_by(phase) %>%
  summarise("pheno_obs#" = length(phase))

######################################################################
# update May-26, 2021 ----
# count the number of observations of each phenophase made on each species 
pheno_spp_obs <- d %>%
  group_by(phase,Common_Name) %>%
  summarise("pheno_spp_obs#" = length(phase))

# count the number of observations of each phenophase made on each individual 
pheno_indiv_obs <- d %>%
  group_by(phase,id) %>%
  summarise("pheno_indiv_obs#" = length(phase))

# quickly linking Individual ID to scientific names
pheno_indiv_obs <- full_join(pheno_indiv_obs,dplyr::select(d, c(Common_Name, id))) %>%  unique()

# update on June-2, 2021
# counting using status intensity data
write.csv(pheno_spp_obs,file = "output/observation_pheno_spp_update_June_2.csv",row.names=FALSE)
write.csv(pheno_indiv_obs,file = "output/observation_pheno_indiv_update_June_2.csv",row.names=FALSE)



d <- full_join(d,indiv_obs)
d <- full_join(d,spp_obs)
d <- full_join(d,pheno_obs)
d <- full_join(d,pheno_spp_obs)
d <- full_join(d,pheno_indiv_obs)


# join the genus and the species columns ----
d$Scientific_Names <- with(d, paste(genus, species, sep = " "))

# add in scientific names into spp_obs and indiv_obs ----
scientific_names <- dplyr::select(d, c(Scientific_Names, Common_Name, id))
scientific_names <- unique(scientific_names)

indiv_obs <- full_join(scientific_names,indiv_obs)
spp_obs <- full_join((scientific_names %>% 
                        dplyr::select(-id) %>% 
                        unique()),spp_obs)

# export ----
write.csv(d,file = "output/observation_table_all_June_2.csv",row.names=FALSE)
write.csv(indiv_obs,file = "output/observation_individual_trees_June_2.csv",row.names=FALSE)
write.csv(spp_obs,file = "output/observation_species_June_2.csv",row.names=FALSE)
write.csv(pheno_obs,file = "output/observation_pheno_June_2.csv",row.names=FALSE)
write.csv(route_obs,file = "output/observation_routes_June_2.csv",row.names=FALSE)
