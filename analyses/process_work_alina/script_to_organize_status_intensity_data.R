# script to organize Status and Intensity data
# June 2
# alina.zeng(at)ubc.ca

# hmmm update on June-2, just realized I should use Status and Intensity data 
# rather than individual phenometrics data to document the frequence of observation


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)


# Set Working Directory ----
setwd("C:/Users/alina/Documents/git/Tree_Spotters")

# update on June 2
d <- read.csv("input/status_intensity_observation_data.csv", header = TRUE)
# note that this document is huge... I will not be pushing it to github therefore
# 334180 obs


# rename the columns to make them more digestible ----
bb <- rename(d, phase=Phenophase_Description, 
             id=Individual_ID, genus=Genus, species=Species, 
             observerID = ObservedBy_Person_ID, route = Site_Name)

# make a column for year
bb$Observation_Date <- as.Date(bb$Observation_Date)

bb <- mutate (bb,
                year = as.numeric(format(bb$Observation_Date, format = "%Y")),
                month = as.numeric(format(bb$Observation_Date, format = "%m")),
                day = as.numeric(format(bb$Observation_Date, format = "%d")))

## subset and adjust the names of the phases
bb.pheno <- dplyr::select(bb, genus, species, Common_Name, phase, 
                          year, id, route, observerID, Plant_Nickname, Phenophase_Status)
## if bb.pheno$phase=="Breaking leaf buds", change it to "budburst", otherwise keep it as it is
## the same goes for the codes below
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)


# figure out which ones are valid, which ones are not
# 1 ["yes the phenophase is occuring"]|0 ["no the phenophase is not occuring"]
# |-1 ["not certain whether the phenophase is occuring"]
occurence <- table(unlist(bb.pheno$Phenophase_Status))

occurence <- occurence %>% mutate(status = 
                        case_when(Var1 == "1" ~ "Yes, the phenophase is occuring",
                                  Var1 == "0" ~ "No, the phenophase is not occuring",
                                  Var1 == "-1" ~ "Unsure"))

# rename columns
occurence <- rename(occurence, "frequency" = "Freq", "indicator" = "Var1")

# export occurrence table
write.csv(occurence, file = "output/occurence_of_observation_status_June_2", row.names = F)