# script_to_subset_data_for_lizzie
# June-4, 2021
# alina.zeng(at).ubc.ca

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(lubridate)

# import data

d <- read.csv("input/status_intensity_observation_data.csv", header = TRUE)

# hmmm hold on, perhap I should use individual phenometrics dataset
d <- read.csv("input/individual_phenometrics_data_all_columns.csv", header = TRUE)


bb <- rename(d, lat=Latitude,long=Longitude,elev=Elevation_in_Meters, 
             year=First_Yes_Year, month=First_Yes_Month, 
             day=First_Yes_Day, doy=First_Yes_DOY, 
             numYs=Multiple_Observers, phase=Phenophase_Description, 
             id=Individual_ID, genus=Genus, species=Species, 
             observerID = ObservedBy_Person_ID, route = Site_Name)

## subset and adjust the names of the phases
bb.pheno <- dplyr::select(bb, genus, species, Common_Name, phase, 
                          year, doy, numYs, id, route, observerID)
## if bb.pheno$phase=="Breaking leaf buds", change it to "budburst", otherwise keep it as it is
## the same goes for the codes below
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)


# join the genus and the species columns ----
bb.pheno$Scientific_Names <- with(bb.pheno, paste(genus, species, sep = " "))

# kk, get rid of columns lizzie might not need
unclean <- select(bb.pheno, -c(numYs, observerID, genus, species,route))

# get rid of columns lizzie might not need
unclean<- subset(unclean, unclean$phase %in% c("budburst","leafout"))

# kk, now make a subset of cleaned data



