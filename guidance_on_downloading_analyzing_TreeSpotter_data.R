# Hello Tree Spotters & fellow enthusiasts!
# This script was created to provide you some guidance on 
      # downloading Tree Spotter
      # tidying up the data so you can perform some simple analysis as you wish

# Note that this script has the exact same content as our lovely wiki page here 
# at https://github.com/alinazeng/Tree_Spotters/wiki/How-to-download-tidy-analyze-Tree-Spotters-data
# You you prefer looking at the wiki page, by all means~


# Contacts ----
# Codes in this script were written by Catherine Chamberlain
        # January, 2019 -- cchamberlain(at)g.harvard.edu
# Alina reformatted and annotated the script to facilitate your understanding
        # May, 2021 -- alina.zeng(at)ubc.ca


# Instructions on downloading data ----

# 1) Go to the NPN Data Downloader Tool at: https://data.usanpn.org/observations
#    Go to the Phenology Observation Portal
# 2) Select 'Individual Phenometrics' and press NEXT 
# 3) Set Date range applicable to your question and press 'Set Date' and NEXT
# 4) Select 'Partner Groups' tab on the left: press the + next to 
#    'Botanic Gardens and Arboretums and select 'Arnold Arboretum - Tree Spotters'
#    Press 'Set Groups' and NEXT
# 5) Select 'Output fields' tab on the left and select 'ObservedBy Person ID', 
#    'Multiple Observers','NumYs_in_Series' and `Multiple_FirstY'
# Press 'Set Output Fields' then click the box next to 'I have read and 
#    acknowledged...' on the right and then click 'Download'
# 6) Go to your Downloads and unzip the datasheet_XX.zip. 
#    Move the `individual_phenometrics_data.csv' to /a_folder_of_your_choice/

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(lubridate)

# Set Working Directory ----
setwd("C:/Users/your_name/Documents/a_folder_of_your_choice")


# Import TreeSpotters data and clean :) ----
d<-read.csv("individual_phenometrics_data.csv", header=TRUE)
# importing the csv file and calling it "d" for simplicity

# let us tidy up citizen scienece data

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
# note that we are now calling the new XXXX "bb"
bb <- rename(d, lat=Latitude,long=Longitude,elev=Elevation_in_Meters, 
             year=First_Yes_Year, month=First_Yes_Month, 
             day=First_Yes_Day, doy=First_Yes_DOY, 
             numYs=Multiple_Observers, phase=Phenophase_Description, 
             id=Individual_ID, genus=Genus, species=Species)

## subset and adjust the names of the phases
bb.pheno<-dplyr::select(bb, genus, species, Common_Name, phase, lat, long, elev, 
year, doy, numYs, id)
## if bb.pheno$phase=="Breaking leaf buds", change it to "budburst", otherwise keep it as it is
## the same goes for the codes below
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)

  
# Now we can work on finding day of budburst, etc. ----
bb.pheno<-filter(bb.pheno, numYs>0) # number of observers greater than zero

# Below, we group each individual by phenophase and year to find the first 
# observation of budburst using the slice function 
doy_pheno<-bb.pheno%>% 
  group_by(id, phase, year) %>%   
  slice(which.min(doy))
doy_pheno<-doy_pheno[!duplicated(doy_pheno),]
# group by id, phase, year so that the first Yes will be recorded by 
#       individuals according to phases and years


# Now start building a small data frame with phenophase info ----

colstokeep<-c("genus", "species", "id","year", "phase","lat", "long", "elev", "doy")
phenos<-subset(doy_pheno, select=colstokeep)
phenos<-phenos[!duplicated(phenos),]

# making the table wide based on phases using the spread function ----
phenos<-phenos%>%tidyr::spread(phase, doy)

# renaming some of the columns ----
phenos$fruits <- phenos$Fruits
phenos$col.leaves<-phenos$`Colored leaves`
phenos$leafdrop<-phenos$`leaf drop`

# further subsetting
phenos <- subset(phenos, 
                 select=c("genus", "species", "id", "year", "lat", "long", 
                          "elev", "budburst", "flowers", "fruits", "leafout", 
                          "col.leaves", "leafdrop"))


# need to update (line 111)
