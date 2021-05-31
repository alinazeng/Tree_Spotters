# Script to figure out which routes/species/individuals were most frequently observed
# May-13-2021
# alinazeng(at)ubc.ca

# Update on May-27-2021

# libraries ----
library(dplyr)
library(tidyr)
library(lubridate)

# set working directory ----
setwd("C:/Users/alina/Documents/git/Tree_Spotters")


# Import Tree Spotters data ----
d<-read.csv("input/individual_phenometrics_data_all_columns.csv", header = TRUE)


# refine descriptions
d$Phenophase_Description<-ifelse(d$Phenophase_Description=="Breaking leaf buds", "budburst", d$Phenophase_Description)
d$Phenophase_Description<-ifelse(d$Phenophase_Description=="Leaves", "leafout", d$Phenophase_Description)
d$Phenophase_Description<-ifelse(d$Phenophase_Description=="Flowers or flower buds", "flowers", d$Phenophase_Description)
d$Phenophase_Description<-ifelse(d$Phenophase_Description=="Falling leaves", "leaf drop", d$Phenophase_Description)

# join the genus and the species columns ----
d$Scientific_Names <- with(d, paste(Genus, Species, sep = " "))

# keep columns that we will use ----

d <- dplyr::select(d, c(Genus, Species, Scientific_Names, Common_Name, Individual_ID, 
                       Phenophase_Description, ObservedBy_Person_ID))


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

# add coordinates ### IMPORTANT: see line 139 for updates
# downloaded data from https://arboretum.harvard.edu/explorer/
# import data

treeinfo <- read.csv("input/MyVisit_filtered.csv", header = TRUE)

# rename columns and filter out unwanted ones
treeinfo <- rename(treeinfo,Plant_ID = Plant.ID,
                   Latitude = Garden.Latitude,
                   Longitude = Garden.Longitude,
                   "DBH(cm)" = DBH)

# joining data frames by Plant_ID
d <- full_join(d, treeinfo)

# add route names ----
d <- d %>%   # overwriting our data frame 
  mutate(Route_Name =   # creating our new column
           case_when(Genus == "Fagus" ~ "Beech Route",
                     Genus == "Betula" ~ "Birch Route",
                     Genus == "Carya" ~ "Hickory Route",
                     Genus %in% c("Tilia", "Aesculus") ~ "Linden North Woods Route",
                     Genus == "Acer" ~ "Maple Route",
                     Genus == "Quercus" ~ "Oak Route",
                     Genus %in% c("Vaccinium","Viburnum","Hamamelis") ~ "Shrub Route"))

# Important: need to use Plant_ID to reassign Peters Hill Route
d[d$Plant_ID %in% c("1323-82*A","16611*F","16611*J","16611*K",
                                                      "16611*O","689-2010*A","611-2010*A","22099*A","12651*I","17538*A",
                                                      "1104-81*A"), ]$Route_Name <- "Peters Hill Route"  

# count the number of observations made along each route ----
route_obs <- d %>%
  group_by(Route_Name) %>%
  summarise("route_obs#" = length(Route_Name))

# make a table with species name, coordinates, # of observation, individual ID
d_with_coordinates <- full_join(d_with_coordinates,route_obs)

# count the number of observations made on individual trees and shrubs ----
indiv_obs  <- d %>%
  group_by(Individual_ID) %>%
  summarise("indiv_obs#" = length(Individual_ID))

# count the number of observations made on each species
spp_obs <- d %>%
  group_by(Common_Name) %>%
  summarise("spp_obs#" = length(Common_Name))

# count the number of observations made on each phenophase
pheno_obs <- d %>%
  group_by(Phenophase_Description) %>%
  summarise("pheno_obs#" = length(Phenophase_Description))

######################################################################
# update May-26, 2021 ----
# count the number of observations of each phenophase made on each species 
pheno_spp_obs <- d %>%
  group_by(Phenophase_Description,Scientific_Names) %>%
  summarise("pheno_spp_obs#" = length(Phenophase_Description))

# count the number of observations of each phenophase made on each individual 
pheno_indiv_obs <- d %>%
  group_by(Phenophase_Description,Individual_ID) %>%
  summarise("pheno_indiv_obs#" = length(Phenophase_Description))

# quickly linking Individual ID to scientific names
pheno_indiv_obs <- full_join(pheno_indiv_obs,dplyr::select(d, c(Common_Name, Individual_ID, 
                                                                Scientific_Names))) %>%  unique()

# Export
write.csv(pheno_spp_obs,file = "output/observation_pheno_spp.csv",row.names=FALSE)
write.csv(pheno_indiv_obs,file = "output/observation_pheno_indiv.csv",row.names=FALSE)

# update on May-27, 2021
# counting using uncleaned data
write.csv(pheno_spp_obs,file = "output/observation_pheno_spp_update_May_27.csv",row.names=FALSE)
write.csv(pheno_indiv_obs,file = "output/observation_pheno_indiv_update_May_27.csv",row.names=FALSE)
# sum 14248 obs


# make a table with species name, coordinates, # of observation, individual ID ----
d <- full_join(d,indiv_obs)
d <- full_join(d,spp_obs)
d <- full_join(d,pheno_obs)
d <- full_join(d,pheno_spp_obs)
d <- full_join(d,pheno_indiv_obs)

#### hmm looks like a good place for me to see how much each observer contributed
## perhaps I will use a separate script


# subset

d <- d %>% 
  dplyr::select(-c(ObservedBy_Person_ID)) %>% 
  unique()


# add in scientific names into spp_obs and indiv_obs ----
scientific_names <- dplyr::select(d, c(Scientific_Names, Common_Name, Individual_ID))
scientific_names <- unique(scientific_names)

indiv_obs <- full_join(scientific_names,indiv_obs)
spp_obs <- full_join((scientific_names %>% 
                        dplyr::select(-Individual_ID) %>% 
                        unique()),spp_obs)

# export ----
write.csv(d,file = "output/observation_table_all_May27.csv",row.names=FALSE)
write.csv(indiv_obs,file = "output/observation_individual_trees_May27.csv",row.names=FALSE)
write.csv(spp_obs,file = "output/observation_species_May27.csv",row.names=FALSE)
write.csv(pheno_obs,file = "output/observation_pheno_May27.csv",row.names=FALSE)
write.csv(route_obs,file = "output/observation_routes_May27.csv",row.names=FALSE)


# prepare a single csv to be used for Arcgis ----

# keep columns that we will use 
final_df <- dplyr::select(d, -c(Genus, Species, 
                        ObservedBy_Person_ID, Phenophase_Description, "pheno_obs#", 
                        Plant_Nickname, "pheno_spp_obs#","pheno_indiv_obs#")) 
# Condense                  
final_df  <- unique((final_df ))
# get rid of retired beech trees
final_df_cleaned <- subset(final_df,final_df$Individual_ID != "86273")
final_df_cleaned <- subset(final_df_cleaned,final_df_cleaned$Individual_ID != "86275")
final_df_cleaned <- subset(final_df_cleaned,final_df_cleaned$Individual_ID != "86277")

# export
write.csv(final_df_cleaned,file = "output/table_for_gis_mapping_May_27.csv",row.names=FALSE)


# test to see if route names are correct
routes_tree <- final_df_cleaned %>%
  group_by(Route_Name) %>%
  summarise("route_tree#" = length(Route_Name))