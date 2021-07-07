# script_to_clean_differently
# alina.zeng(at)ubc.ca
# July-1, 2021


# update on July-6, this no longer works because the old cleaning approach is accepting
# "0s" as valid observations too. 
# jump to line 136


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




# July 06, 2021, importing raw status intensity data
raw <- read.csv("input/status_intensity_observation_data_all_columns.csv", header = TRUE)

# select only columns we need

subraw<-subset(raw, select= c( "Observation_ID",  "ObservedBy_Person_ID",  "Observation_Date" ,
                                     "Site_Name", "Genus","Species" , "Common_Name"  , "Individual_ID", 
                                     "Phenophase_Description" ,   "Day_of_Year" , "Phenophase_Status", "year"))							

# rename
subraw <- rename(subraw,observation_id = Observation_ID,treespotter_id = ObservedBy_Person_ID,
                 observation_date = Observation_Date, route = Site_Name, genus = Genus, 
                 species = Species,common_name = Common_Name,tree_id = Individual_ID, 
                 phase = Phenophase_Description,   
                 doy = Day_of_Year, status = Phenophase_Status)

subraw$phase<-ifelse(subraw$phase=="Breaking leaf buds", "budburst", subraw$phase)
subraw$phase<-ifelse(subraw$phase=="Leaves", "leafout", subraw$phase)
subraw$phase<-ifelse(subraw$phase=="Flowers or flower buds", "flowers", subraw$phase)
subraw$phase<-ifelse(subraw$phase=="Falling leaves", "leaf drop", subraw$phase)



# test out how many "1" doys were recorded by multiple people

# keke, get rid of "-1s" first
subraw <- subset(subraw,subraw$status != "-1")

subraw <- subraw %>% group_by(year, phase, tree_id, doy) %>% mutate(multiple_observer = sum(status))


# hmm okay... lemme see what happens if I get rid of the 1s and 0s?
no0no1 <- subset(subraw,subraw$multiple_observer != "0" & subraw$multiple_observer != "1")

# hmmm and try taking the mean of the doy
# see how many observatons there is..
summ_no0no1 <- no0no1 %>% group_by(common_name,year, phase, tree_id) %>% 
  summarise(doy_mean=mean(doy), doy_median = median(doy),obs_number=length(doy),first_doy = min(doy),
            last_doy = max(doy),interquartile_range = IQR(doy))


# hmm lemme find the fisrt doys of each phase
no0 <- subset(subraw,subraw$multiple_observer != "0")
summ_no0 <- no0 %>% group_by(common_name,year, phase, tree_id) %>% 
  summarise(doy_mean=mean(doy), doy_median = median(doy),obs_number=length(doy),first_doy = min(doy),
            last_doy = max(doy),interquartile_range = IQR(doy))


# export data for lizzie to see first
write.csv(summ_no0no1, file = "output/observations_excluding_single_observers_July6.csv", row.names =  F)
write.csv(summ_no0, file = "output/observations_including_single_observers_July6.csv", row.names =  F)



# continue to work on this July 7
# i think i want to... assign an indicator to good rows, later on i can get rid of ones that are out of the range, and dont hv multiple observers)

dayrange <- 5
indicator <- list() # empty vector to use in loop
for (i in c(1:length(no0$doy))){
  dayz <- seq(no0$doy[i]-dayrange, no0$doy[i]+dayrange) # set up a vector of allowed days
 # sumhere <- sum(df$obsYESNO[which(df$doy %in% dayz)]) # this should sum up observations in that range, indexing one column based on conditions in another column
   if(no0$doy %in% dayz){
   withinrange <- 1}  
   dat <- data.frame(no0$observation_id,withinrange)  
   indicator[[i]] <- dat
  }
  
  
  
    indicator <- 
  
  if(sumhere>1){ 
    goodrows <- c(goodrows, i) # trying to record which rows > 1 [see](https://www.datamentor.io/r-programming/if-else-statement/) though there is probably a nicer way to add this; you could also write out a dataframe of the sums so you can see how many are 1, 2, 3 or more.
  }}



# update on July 07, getting a bit creative
# after exporting raw and subraw
# get rid of "-1s" and "0s" first
subraw <- subset(subraw,subraw$status != "-1" & subraw$status != "0")

# order doy based on groups 
subraw <- subraw   %>% 
  group_by(year, phase, tree_id) %>%
  arrange(doy, .by_group = TRUE)

# calculate the difference
subraw <- subraw  %>% group_by(year, phase, tree_id)%>%
  mutate(difference = doy - lag(doy,default=first(doy)))

# add number of observations
subraw <- subraw  %>% group_by(year, phase, tree_id, doy) %>% 
  mutate(obs_num = sum(status))

# filter out extremes
clean<- filter(subraw,difference < 6 |obs_num > 1)

# quickly calcultate max and min and range
summ_clean <- clean %>% group_by(common_name,year, phase, tree_id) %>% 
  summarise(doy_mean=mean(doy), doy_median = median(doy),obs_number=length(doy),first_doy = min(doy),
            last_doy = max(doy),interquartile_range = IQR(doy))

# get rid of 2015
summ_clean <- subset(summ_clean,summ_clean$year != "2015")

# export for lizzie to see
write.csv(summ_clean, file = "output/cleaningJuly07.csv", row.names = F)