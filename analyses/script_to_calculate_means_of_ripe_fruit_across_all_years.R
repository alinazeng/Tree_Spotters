# script to calculate means of ripe fruit across all years
# May-21-2021

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load Libraries
library(dplyr)
library(tidyr)
library(lubridate)

# set working directory
setwd("C:/Users/alina/Documents/git/Tree_Spotters")

# Get treespotters data - and clean
d<-read.csv("input/individual_phenometrics_data.csv", header=TRUE) 

### First let's do the obligatory cleaning checks with citizen scienece data
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] ## This selects data where multiple people observed the same phenophase
d <- d[(d$NumYs_in_Series>=3),] ## This selects data again where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] ## And this limits to data where a no is followed by a yes, so that it is a new observation/new phenophase but has been detected within a reasonable timeframe
# 4156 obs

# make column names more digestible
bb <- rename(d, lat=Latitude,long=Longitude,elev=Elevation_in_Meters, 
             year=First_Yes_Year, month=First_Yes_Month, 
             day=First_Yes_Day, doy=First_Yes_DOY, 
             numYs=Multiple_Observers, phase=Phenophase_Description, 
             id=Individual_ID, genus=Genus, species=Species)

bb.pheno<-dplyr::select(bb, genus, species, Common_Name, phase, lat, long, elev, year, doy, numYs, id)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)
## if bb.pheno$phase=="Breaking leaf buds", change it to "budburst", or keep it as it is

### Now work on finding day of budburst, etc.
bb.pheno<-filter(bb.pheno, numYs>0) # number of observers greater than zero
# Below, I group each individual by phenophase and year to find the first observation (using the slice function), 
## so first day of budburst for that individual for that year
doy_pheno<-bb.pheno%>% 
  group_by(id, phase, year) %>%   ## group by id, phase, year so that the first Yes will be recorded by individuals according to phases and years
  slice(which.min(doy))
doy_pheno<-doy_pheno[!duplicated(doy_pheno),]


#### Now start building a small data frame with phenophase info then add in climandpheno, chilling and photo
colstokeep<-c("genus", "species", "id","year", "phase","lat", "long", "elev", "doy")
phenos<-subset(doy_pheno, select=colstokeep)

phenos<-phenos[!duplicated(phenos),]

# make the table long
phenos<-phenos%>%tidyr::spread(phase, doy)

phenos$ripe_fruit <- phenos$`Ripe fruits`
phenos$recent_fruit_drop <- phenos$`Recent fruit or seed drop`


# subset
phenos_fruit <- subset(phenos, select=c("genus", "species", "id", "year", 
                                        "ripe_fruit", "recent_fruit_drop"))

# Join the genus and the species columns ----
phenos_fruit$scientific_names <- with(phenos_fruit, paste(genus, species, sep = " "))


# Calculate means doy of ripe fruit and fruit drop
summary_mean_ripe <- phenos_fruit %>% 
  group_by(scientific_names) %>% 
  summarise(ripe_mean = mean(ripe_fruit, na.rm=TRUE)) 
summary_mean_drop <- phenos_fruit %>% 
  group_by(scientific_names) %>% 
  summarise(fruitdrop_mean = mean(recent_fruit_drop, na.rm=TRUE)) 

# joining the two tables
fruit_mean <- full_join(summary_mean_drop,summary_mean_ripe)

# export 
write.csv(fruit_mean, file = "output/treespotters_pheno_fruit_means_across_5_years", row.names = F)


