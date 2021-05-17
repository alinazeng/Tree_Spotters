# This script will be used to figure out which routes/species/individuals were most frequently observed
# Development is underway
# May-13-2021
# alinazeng(at)ubc.ca


# can represent some info using pie charts 
# eg. out of 300000 records, 40% (120000 were made on XXX route)
# http://data.usanpn.org/npn-viz-tool/


# also need to work on mapping using GIS
# need to download coordinates from this site: https://arboretum.harvard.edu/explorer/
# clean/format csv. files and then map
# http://map.arboretum.harvard.edu/arcgis/rest/services/CollectionExplorer/MapServer


# hmmmmm documenting my to-do-list for Monday, May 17
# calculate observations -> by individual, spp, route, across phenophases
# download coordinates data and compile everything together
# meet cat <333


# libraries
library(dplyr)
library(tidyr)
library(lubridate)

<<<<<<< HEAD
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

# count numbers of observations by individual
test <- d %>%
  group_by(Individual_ID) %>%
  summarise("indiv_obs#" = length(Individual_ID))

# make a table with species name, coordinates, # of observation, individual ID
d2 <- full_join(d,test)

# need to rewrite the script when I'm done
d2 <- dplyr::select(d2, -c(Site_ID, State,Kingdom,First_Yes_Julian_Date,NumDays_Since_Prior_No,Last_Yes_Year,Last_Yes_Month, Last_Yes_Day,Last_Yes_DOY, Last_Yes_Julian_Date,NumDays_Until_Next_No, NumYs_in_Series,numYs,Multiple_FirstY))
d2 <- rename(d2, Individual_ID = id)
d2 <- full_join(d2,test)

# calculate observation number by species and join
test2 <- d %>%
  group_by(Common_Name) %>%
  summarise("spp_obs#" = length(Common_Name))

d2 <- full_join(d2,test2)

# Join the genus and the species columns ----
d2$scientific_names<- with(d2, paste(genus, species, sep = " "))

# duplicate genus column first

case_when(genus == "Fagus" ~ "Beech Route",
          genus == "Betula" ~ "Birch Route",
          genus == "Carya" ~ "Hickory Route"
          genus %in% c("Tilia", "Aesculus") ~ "Linden North Woods Route",
          genus == "Acer" ~ "Maple Route",
          genus == "Quercus" ~ "Oak Route",
          vector2 == "D" ~ "joob")
=======
  # casually updating
  >>>>>>> 4fd37319b253f0855ba0690b240690ef7f326156
