# Hello Tree Spotters & fellow enthusiasts!
# This script was created to provide you some guidance on 
# downloading Tree Spotter data
# tidying up the data so you can perform some simple analysis as you wish

# Note that this script has the exact same content as our lovely wiki page here 
# at https://github.com/alinazeng/Tree_Spotters/wiki/How-to-download-tidy-analyze-Tree-Spotters-data
# If you prefer looking at the wiki page, by all means~

# Contact ----
# alina.zeng(at)ubc.ca

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
# Year 2016-2021 in this repository: Tree_Spotters/input/individual_phenometrics
# You can view the description of column names here: Tree_Spotters/input/individual_phenometrics_datafield_descriptions.xlsx

d <- read.csv("input/individual_phenometrics_data.csv", header = TRUE)
# importing the csv file and calling it "d" for simplicity

# let us tidy up citizen science data
# note: everyting in this script suggests what you can do, not necessarily what 
# you have to do; please feel free to use commands that are applicable to your interest


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

# let us rename the columns to make them more digestible ----
# note that we are now calling the new data frame "bb"
bb <- rename(d, lat=Latitude,long=Longitude,elev=Elevation_in_Meters, 
             year=First_Yes_Year, month=First_Yes_Month, 
             day=First_Yes_Day, doy=First_Yes_DOY, 
             numYs=Multiple_Observers, phase=Phenophase_Description, 
             id=Individual_ID, genus=Genus, species=Species)


## subset and adjust the names of the phases
bb.pheno <- dplyr::select(bb, genus, species, Common_Name, phase, lat, long, elev, 
                          year, doy, numYs, id)
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


#### Now start building a small data frame with phenophase info 
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


### Now clean it up a bit if you want to get rid of the NAs (optional)
phenos<-phenos[!is.na(phenos$budburst),]
phenos<-phenos[!is.na(phenos$leafout),]
# etc.


# update on May-18, 2021
# looks like something fishy is going on with Quercus 2016 data 
phenos_cleaned <- subset(phenos,phenos$leafout != "282")



# export file 
write.csv(phenos_cleaned, file="output/clean_treespotters_allphenodata.csv", row.names=FALSE)

# now you can perform analyses as you'd like on this dataframe you just exported
# feel free to check out the scripts in analyses/process_work_alina to see what 
# other things you could do