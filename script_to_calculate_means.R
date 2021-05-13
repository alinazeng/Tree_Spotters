# May-12-2021
# Alina's first attempt to calculate the means of species x phenophases
# and it worked!!
Tree_Spotters

# update on May-13-2021
# I just realized I probably should also group species by year to see the change
# in DOY over years. Will do right now



# set working directory ----
setwd("C:/Users/alina/Documents/git/Tree_Spotters")


# Load Libraries ----
library(dplyr)
library(tidyr)
library(lubridate)

# import phenodata sheet produced using Cat's script ----
phenodata <- read.csv("clean_treespotters_allphenodata_May11_2.csv", header=TRUE)
# for folks who would like to follow along from here, this csv file can be found in Tree_Spotters repository 
# at https://github.com/alinazeng/Tree_Spotters/blob/main/clean_treespotters_allphenodata_May11_2.csv

# Join the genus and the species columns ----
phenodata$scientific_names<- with(phenodata, paste(genus, species, sep = " "))

# Attention: if you want to calculate the means across multiple years, please 
# group data by scientific names ----
phenodata_grouped <- group_by(phenodata, scientific_names)

# If you are calculating the means of each year individually, please
# group data by both scientific names and year
phenodata_grouped <- group_by(phenodata, scientific_names, year)


# make a new table for means ----

summary_bb <- summarise(phenodata_grouped, bb_mean = mean(budburst, na.rm=TRUE))
summary_flower <- summarise(phenodata_grouped, flower_mean = mean(flowers, na.rm=TRUE))
summary_fruit <- summarise(phenodata_grouped, fruit_mean = mean(fruits, na.rm=TRUE))
summary_leafout <- summarise(phenodata_grouped, leafout_mean = mean(leafout, na.rm=TRUE))
summary_col.leaves <- summarise(phenodata_grouped, col.leaves_mean = mean(col.leaves, na.rm=TRUE))
summary_leafdrop <- summarise(phenodata_grouped, leafdrop_mean = mean(leafdrop, na.rm=TRUE))
summary_last_obs<- summarise(phenodata_grouped, last_obs_mean = mean(last.obs, na.rm=TRUE))

# join them all by scientific names ----
# I know this is a hideous code :')))) someone please help me beatify it lolllllllll
pheno_means <- (full_join(full_join(full_join(full_join(full_join
                        (full_join(summary_bb, summary_flower),summary_fruit), 
                         summary_leafout), summary_col.leaves),summary_leafdrop), 
                         summary_last_obs ))

# check out our lovely mean table ----
str(pheno_means)

# round up the dates ----
pheno_means$bb_mean <- round(pheno_means$bb_mean, digits=0)
pheno_means$flower_mean <- round(pheno_means$flower_mean, digits=0)
pheno_means$fruit_mean <- round(pheno_means$fruit_mean, digits=0)
pheno_means$leafout_mean <- round(pheno_means$leafout_mean, digits=0)
pheno_means$col.leaves_mean <- round(pheno_means$col.leaves_mean, digits=0)
pheno_means$leafdrop_mean <- round(pheno_means$leafdrop_mean, digits=0)
pheno_means$last_obs_mean <- round(pheno_means$last_obs_mean, digits=0)

# yay I realized this one liner works the same. Learning something every day hooray ~~
pheno_means <- pheno_means %>% mutate_if(is.numeric, ~round(., 0))               # https://stackoverflow.com/questions/27613310/rounding-selected-columns-of-data-table-in-r


# let's look at the last observations ----
# some of the last_obs_mean is smaller than leafdrop_mean and we need to fix this
pheno_means$last_obs_mean <- ifelse(
  pheno_means$last_obs_mean < pheno_means$leafdrop_mean, 
  pheno_means$leafdrop_mean, pheno_means$last_obs_mean )
# Yayyy it worked!
# the ifelse function sets it that the last observation doy will equate to 
# leafdrop doy if it is smaller than the leafdrop doy, otherwise no change


# export to csv ----
write.csv(pheno_means, file="treespotters_pheno_means.csv", row.names=FALSE)


# you can find the exported csv within this repository here 
# at https://github.com/alinazeng/Tree_Spotters/blob/main/treespotters_pheno_means_across_5_years.csv  (for means over 5 years)
# at https://github.com/alinazeng/Tree_Spotters/blob/main/treespotters_pheno_means_individual_year.csv (for means of each individual year)

# hmmm should I calculate the timespan between phases using R or excel...
# lemme ponder

# also I need to think about how to visualize the means across species
