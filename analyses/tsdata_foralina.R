### 24 January 2019 - Cat
## Fresh start to cleaning phenology data
# Then more scripts, to calculate chilling and forcing

## Tree Spotters Data download: https://data.usanpn.org/observations/get-started
## Individual Phenometrics -> Set Date -> Set Partner Groups -> Output fields (ObservedBy Person ID, Multiple Observers)

### 24 January 2019 - Cat
## Clean Treespotters data


## How to download data:
# 1) Go to the NPN Data Downloader Tool: https://data.usanpn.org/observations
# And go to the Phenology Observation Portal
# 2) Select 'Individual Phenometrics' and press NEXT 
# 3) Set Date range applicable to your question and press 'Set Date' and NEXT
# 4) Select 'Partner Groups' tab on left: press the + next to 
# 'Botanic Gardens and Arboretums and select 'Arnold Arboretum - Tree Spotters'
# Press 'Set Groups' and NEXT
# 5) Select 'Output fields' tab on left: and select 'ObservedBy Person ID', 'Multiple Observers',
## `NumYs_in_Series' and `Multiple_FirstY'
# Press 'Set Output Fields' then click the box next to 'I have read and acknowledged...' on the right and then click 'Download'
# 6) Go to your Downloads and unzip the datasheet_XX.zip. Move the `individual_phenometrics_data.csv' to analyses/input/


# ## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(lubridate)


# Set Working Directory
setwd("~/Documents/git/microclimates/analyses")
setwd("C:/Users/alina/Documents/git/Tree_Spotters")


# 1. Get treespotters data - and clean!
d<-read.csv("input/individual_phenometrics_data.csv", header=TRUE) ## 24 Jan 2019: 7671
d<-read.csv("individual_phenometrics_data.csv", header=TRUE) 


### First let's do the obligatory cleaning checks with citizen scienece data
d <- d[(d$Multiple_FirstY>=1 | d$Multiple_Observers>0),] ## This selects data where multiple people observed the same phenophase
d <- d[(d$NumYs_in_Series>=3),] ## This selects data again where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] ## And this limits to data where a no is followed by a yes, so that it is a new observation/new phenophase but has been detected within a reasonable timeframe
# 4156 obs


# change from NPN output to more digestible column names
bb<-d%>%
  rename(lat=Latitude)%>%
  rename(long=Longitude)%>%
  rename(elev=Elevation_in_Meters)%>%
  rename(year=First_Yes_Year)%>%
  rename(month=First_Yes_Month)%>%
  rename(day=First_Yes_Day)%>%
  rename(doy=First_Yes_DOY)%>%
  rename(numYs=Multiple_Observers)%>%
  rename(phase=Phenophase_Description)%>%
  rename(id=Individual_ID)%>%
  rename(genus=Genus)%>%
  rename(species=Species)

# equivalent to
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

### Clean observation error!
if(FALSE){ #Detected with new cleaning checks!! 3 July 2019 by Cat
  # QbyLizzie: How did you find this? - I found this by making initial raw data plots. There was a outlier and I went back 
  ## to check the data. It was a new volunteer who made a couple of mistakes. 
  doy_pheno$doy<-ifelse(doy_pheno$species=="alleghaniensis" & doy_pheno$year==2016 & doy_pheno$doy==59, NA, doy_pheno$doy)
  doy_pheno<-doy_pheno[!is.na(doy_pheno$doy),]
}

#### Now start building a small data frame with phenophase info then add in climandpheno, chilling and photo
colstokeep<-c("genus", "species", "id","year", "phase","lat", "long", "elev", "doy")
phenos<-subset(doy_pheno, select=colstokeep)

phenos<-phenos[!duplicated(phenos),]

phenos<-phenos%>%tidyr::spread(phase, doy)

phenos$fruits <- phenos$Fruits
phenos$col.leaves<-phenos$`Colored leaves`
phenos$leafdrop<-phenos$`leaf drop`

phenos <- subset(phenos, select=c("genus", "species", "id", "year", "lat", "long", "elev", "budburst", 
                                  "flowers", "fruits", "leafout", "col.leaves", "leafdrop"))

### And now last observation for when to start calculating chilling
phenos$last.obs<-ave(phenos$leafdrop, phenos$id, phenos$year, FUN=last)
## ave()-> looks at all the arguments it contains, and call the function at this end
phenos$last.obs<-ifelse(is.na(phenos$last.obs), ave(phenos$col.leaves, phenos$id, phenos$year, FUN=last), phenos$last.obs)
# if last observation for leaf drop is NA, then record the last day for leaf changing color


## For gdd start and chilling end
phenos$gdd.start<-46 # 15 February for each year - arbitrary, can change


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


# export file 
write.csv(phenos_cleaned, file="output/clean_treespotters_allphenodata_May_18.csv", row.names=FALSE)



prov <- read.csv("output/provenanceinfo.csv", header=TRUE)
# if something weird happens
prov <- mutate(prov,Individual_ID=prov$Individual_ID)

prov <- subset(prov, select= c("Individual_ID", "provenance.lat", "provenance.long"))
prov <- prov %>% rename(id=Individual_ID)

ts <- full_join(prov, phenos)

colstokeep <- c("id", "provenance.lat", "provenance.long", "genus", "species", "year", "budburst", "leafout", "last.obs", "yr.end", "gdd.start", "type")
d <- subset(ts, select=colstokeep)
d <- d[!is.na(d$genus),]

d$id <- as.character(d$id)

# write.csv(phenos, file="output/clean_treespotters_allphenodata_plus_provenance.csv", row.names=FALSE)

