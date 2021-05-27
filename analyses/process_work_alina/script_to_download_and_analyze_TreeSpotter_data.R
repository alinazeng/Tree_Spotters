# This document outlines how Alina downloaded and subset data for specific analysis purposes 
# update on May-27, 2021 to make script cleaner


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(lubridate)

# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/Tree_Spotters")

# Import Tree Spotters data and clean ----
d <- read.csv("input/individual_phenometrics_data_all_columns.csv", header = TRUE)
# importing the csv file and calling it "d" for simplicity

# let us tidy up citizen science data (optional)
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] 
      # This selects data where multiple people observed the same phenophase
d <- d[(d$NumYs_in_Series>=3),] 
      # This selects data where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] 
      # This limits to data where a no is followed by a yes, so that it is a new
      # observation/new phenophase but has been detected within a fair timeframe

# you can view d to make sure nothing weird is going on ----
    view(d)
    str(d)
    dim(d)
    summary(d) # a few ways to get ahold of the attributes and characteristics 

# rename the columns to make them more digestible ----
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

  
# Now we can work on finding day of budburst, etc. ----
bb.pheno <- filter(bb.pheno, numYs > 0) # number of observers greater than one

# Below, we group each individual by phenophase and year to find the first 
# observation of budburst using the slice function 
doy_pheno <- bb.pheno%>% 
  group_by(id, phase, year) %>%   
  slice(which.min(doy))
doy_pheno <- doy_pheno[!duplicated(doy_pheno),]
# group by id, phase, year so that the first Yes will be recorded by 
#       individuals according to phases and years

# Now start building a small data frame with phenophase info ----

colstokeep <- c("genus", "species", "id","year", "phase", "doy")
phenos <- subset(doy_pheno, select=colstokeep)
phenos <- phenos[!duplicated(phenos),]

# making the table wide based on phases using the spread function ----
phenos <- phenos%>%tidyr::spread(phase, doy)

# renaming some of the columns ----
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

### Now clean it up a bit (optional)
phenos<-phenos[!is.na(phenos$budburst),]
phenos<-phenos[!is.na(phenos$leafout),]
phenos<-phenos[!is.na(phenos$last.obs),]

# update on May-18, 2021
# looks like something fishy is going on with Quercus 2016 data 
phenos_cleaned <- subset(phenos,phenos$leafout != "282")

# export file May-27, 2021 ----
write.csv(phenos_cleaned, file="output/clean_treespotters_allphenodata_with_NAs_May_27.csv", row.names=FALSE)

