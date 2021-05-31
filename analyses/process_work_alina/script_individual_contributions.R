# Script to calculate the contribution of inidvidual observers
# May-21, 2021
# alina.zeng(at)ubc.com



# libraries ----
library(dplyr)
library(tidyr)
library(stringr)


# Import Tree Spotters data ----
d<-read.csv("input/individual_phenometrics_data_all_columns.csv", header = TRUE)


# refine descriptions
d$Phenophase_Description<-ifelse(d$Phenophase_Description=="Breaking leaf buds", "budburst", d$Phenophase_Description)
d$Phenophase_Description<-ifelse(d$Phenophase_Description=="Leaves", "leafout", d$Phenophase_Description)
d$Phenophase_Description<-ifelse(d$Phenophase_Description=="Flowers or flower buds", "flowers", d$Phenophase_Description)
d$Phenophase_Description<-ifelse(d$Phenophase_Description=="Falling leaves", "leaf drop", d$Phenophase_Description)

# join the genus and the species columns ----
d$Scientific_Names <- with(d, paste(Genus, Species, sep = " "))

# keep columns that we will use ----

d <- dplyr::select(d, c(Genus, Species, Scientific_Names, Common_Name, Individual_ID, 
                        Phenophase_Description, ObservedBy_Person_ID))



# experimenting... ----
str_count(d$ObservedBy_Person_ID, "19414")
sum(str_count(d$ObservedBy_Person_ID, "19414"))

# remove quotation marks from column
test<-gsub("'","",as.character(d$ObservedBy_Person_ID))
test2 <- as.numeric(test)
length(unique(test2))
#169 individual observers

sum(is.na(test2))
# 6005 observatsions are group observation
# 14248-6005 are individual observation