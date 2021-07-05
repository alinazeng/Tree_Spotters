# script_to_clean_differently
# alina.zeng(at)ubc.ca
# July-1, 2021


# from lizzie:" Take the 'clean' data and calculate the mean and total range, 
# the allow in all 'unclean' observations that are within 2X of the range. For 
# example, if the mean of the clean is 30, and the range is 23-40 (that's 17 days 
# range), then we would take 30-17 and 30+17 (13 to 47) as the allowable range to 
# include 'unclean' observations."



# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(lubridate)

# import raw data ----
d <- read.csv("input/individual_phenometrics_data_all_columns.csv", header = TRUE)


# 1 export a "clean" file
### First let's do the obligatory cleaning checks with citizen scienece data
d <- d[(d$Multiple_FirstY>=1 & d$Multiple_Observers>0),] ## This selects data where multiple people observed the same phenophase
d <- d[(d$NumYs_in_Series>=3),] ## This selects data again where the same phenophase was seen 3 times in a row
d <- d[(d$NumDays_Since_Prior_No>=0 & d$NumDays_Since_Prior_No<=14),] ## And this limits to data where a no is followed by a yes, so that it is a new observation/new phenophase but has been detected within a reasonable timeframe
# 3612 obs

bb <- rename(d, lat=Latitude,long=Longitude,elev=Elevation_in_Meters, 
             year=First_Yes_Year, month=First_Yes_Month, 
             day=First_Yes_Day, doy=First_Yes_DOY, 
             numYs=Multiple_Observers, phase=Phenophase_Description, 
             id=Individual_ID, genus=Genus, species=Species, observer=ObservedBy_Person_ID)

bb.pheno<-dplyr::select(bb, genus, species, Common_Name, phase, observer, lat, long, elev, year, doy, numYs, id)
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

# join the genus and the species columns ----
bb.pheno$Scientific_Names <- with(bb.pheno, paste(genus, species, sep = " "))

# get rid of columns we might not need
clean <- select(bb.pheno, -c(numYs, genus, lat, long, elev, species))


# 2 export unclean dataset
d <- read.csv("input/individual_phenometrics_data_all_columns.csv", header = TRUE)

bb <- rename(d, lat=Latitude,long=Longitude,elev=Elevation_in_Meters, 
             year=First_Yes_Year, month=First_Yes_Month, 
             day=First_Yes_Day, doy=First_Yes_DOY, 
             numYs=Multiple_Observers, phase=Phenophase_Description, 
             id=Individual_ID, genus=Genus, species=Species, 
             observerID = ObservedBy_Person_ID, route = Site_Name)

## if bb.pheno$phase=="Breaking leaf buds", change it to "budburst", otherwise keep it as it is
## the same goes for the codes below
bb.pheno<-dplyr::select(bb, genus, species, Common_Name, phase, lat, long, elev, year, doy, numYs, id)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)

# join the genus and the species columns ----
bb.pheno$Scientific_Names <- with(bb.pheno, paste(genus, species, sep = " "))

# get rid of columns we might not need
unclean <- select(bb.pheno, -c(numYs, genus, lat, long, elev, species))


# calculate mean and range using "clean" data ----

# across 5 years
summ_clean <- clean %>%  group_by(Common_Name, phase) %>%  
  summarise(mean_clean = mean(doy), max = max(doy, na.rm = T), 
            min = min(doy, na.rm = T),maxmin_range = (max(doy)-min(doy)), 
            interquartile_range= IQR(doy))

# looking at each  year individually
summ_clean_year <- clean %>%  group_by(Common_Name, phase, year) %>%  
  summarise(mean_clean = mean(doy), 
            max = max(doy, na.rm = T), min = min(doy, na.rm = T),
            maxmin_range = (max(doy)-min(doy)), interquartile_range= IQR(doy))

# hmm adding in numbers of observations 
summ_clean_year2  <- clean %>%  group_by(Common_Name, phase, year) %>%  
  summarise(mean_clean = mean(doy), number_obs = length(doy[!is.na(doy)]), max = max(doy, na.rm = T), min = min(doy, na.rm = T),
            maxmin_range = (max(doy)-min(doy)), interquartile_range= IQR(doy))


# look at 2019 basswood
basswood2019 <- subset(clean,clean$Common_Name == "American basswood" 
                       & clean$year == 2019 & clean$phase == "budburst")
# ascend
basswood2019 <- basswood2019 %>% arrange(desc(basswood2019$doy))

basswood2019<- basswood2019[with(basswood2019, order(doy)), ]


write.csv(basswood2019, file = "output/observer_basswood2019_July2.csv", row.names = F)

write.csv(observer_year, "output/number_of_observation_by_each_individual_each_year_June_2.csv",row.names = F)


#hmmm very problematic , maybe its better if we go with option 3

write.csv(summ_clean_year, file = "output/problematic_clean_range_July1.csv", row.names = F)


# experimenting
test <- summ_clean_year %>% group_by(Common_Name, phase) %>% 
  summarize(max2 = max(mean_clean, na.rm = T), min2 = min(mean_clean, na.rm = T),
                    maxmin_range2 = (max(mean_clean)-min(mean_clean)) )
