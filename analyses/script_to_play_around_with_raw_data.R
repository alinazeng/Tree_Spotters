# script to play around with raw data
# May-27, 2021


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(lubridate)

# Set Working Directory ----
setwd("C:/Users/your_name/Documents/git/Tree_Spotters")

# Import Tree Spotters data

d <- read.csv("input/individual_phenometrics_data_all_columns.csv", header = TRUE)
# 14248 observations
# importing the csv file and calling it "d" for simplicity

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


### Now work on finding day of budburst, etc.
bb.pheno<-filter(bb.pheno, numYs>0) # number of observers greater than one
# Below, I group each individual by phenophase and year to find the first observation (using the slice function), 
## so first day of budburst for that individual for that year
doy_pheno<-bb.pheno%>% 
  group_by(id, phase, year) %>%   ## group by id, phase, year so that the first Yes will be recorded by individuals according to phases and years
  slice(which.min(doy))
doy_pheno<-doy_pheno[!duplicated(doy_pheno),]


#### Now start building a small data frame with phenophase info then add in climandpheno, chilling and photo
colstokeep<-c("genus", "species", "id","year", "phase", "doy")
phenos<-subset(doy_pheno, select=colstokeep)

phenos<-phenos[!duplicated(phenos),]

phenos<-phenos%>%tidyr::spread(phase, doy)

phenos$fruits <- phenos$Fruits
phenos$col.leaves<-phenos$`Colored leaves`
phenos$leafdrop<-phenos$`leaf drop`
phenos$flower_open<-phenos$`Open flowers`
phenos$flower_pollen <- phenos$`Pollen release (flowers)`
phenos$fruit_ripe <- phenos$`Ripe fruits`
phenos$fruit_drop <- phenos$`Recent fruit or seed drop`

phenos <- subset(phenos, select=c("genus", "species", "id", "year", "budburst", 
                                  "flowers", "fruits", "leafout", "col.leaves", "leafdrop",
                                  "flower_open","flower_pollen","fruit_ripe","fruit_drop"))


### Now clean it up a bit
phenos<-phenos[!is.na(phenos$budburst),]
phenos<-phenos[!is.na(phenos$leafout),]
phenos<-phenos[!is.na(phenos$last.obs),]
# do we not want to get rid of the NAs for other things? if not, how do we interpret those?

phenos$yr.end <- ifelse(phenos$year==2016, 366, 365)
phenos$yr.end <- ifelse(phenos$year==2016 |phenos$year==2020, 366, 365)
phenos$yr.end <- ifelse(phenos$year%in%c(2016, 2020), 366, 365)

## And now add in individual information...
phenos$type <- "Treespotters"


# update on May-18, 2021
# looks like something fishy is going on with Quercus 2016 data 
phenos_cleaned <- subset(phenos,phenos$leafout != "282")


# rename columns
phenos_cleaned <- rename(phenos_cleaned, )

# export file 
write.csv(phenos_cleaned, file="output/clean_treespotters_allphenodata_with_NAs_May_27.csv", row.names=FALSE)

