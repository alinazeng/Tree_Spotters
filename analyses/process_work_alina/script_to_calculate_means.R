# Calculate the means of species x phenophases
# May-12-2021
# alina.zeng(at)ubc.ca



# set working directory ----
setwd("C:/Users/alina/Documents/git/Tree_Spotters")

# Load Libraries ----
library(dplyr)
library(tidyr)
library(lubridate)

# import phenodata sheet produced using Cat's script ----
phenodata <- read.csv("output/clean_treespotters_allphenodata_May_18.csv", header = TRUE)

# update on May-27, 2021 using data with NAs
phenodata <- read.csv("output/clean_treespotters_allphenodata_with_NAs_May_27.csv", header = TRUE)
# 443 obs

# Join the genus and the species columns ----
phenodata$scientific_names <- with(phenodata, paste(genus, species, sep = " "))

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


summary_bb <- summarise(phenodata_grouped, bb_mean = mean(budburst, na.rm=TRUE))
summary_flower <- summarise(phenodata_grouped, flower_mean = mean(flowers, na.rm=TRUE))
summary_fruit <- summarise(phenodata_grouped, fruit_mean = mean(fruits, na.rm=TRUE))
summary_leafout <- summarise(phenodata_grouped, leafout_mean = mean(leafout, na.rm=TRUE))
summary_col.leaves <- summarise(phenodata_grouped, col.leaves_mean = mean(col.leaves, na.rm=TRUE))
summary_leafdrop <- summarise(phenodata_grouped, leafdrop_mean = mean(leafdrop, na.rm=TRUE))
summary_flower_open<- summarise(phenodata_grouped, flower_open_mean = mean(flower_open, na.rm=TRUE))
summary_flower_pollen <- summarise(phenodata_grouped, flower_pollen_mean = mean(flower_pollen, na.rm=TRUE))
summary_fruit_ripe <- summarise(phenodata_grouped, fruit_ripe_mean = mean(fruit_ripe, na.rm=TRUE))
summary_fruit_drop <- summarise(phenodata_grouped, fruit_drop_mean = mean(fruit_drop, na.rm=TRUE))


# join them all by scientific names ----
# I know this is a hideous code :')))) someone please enlighten me haha
pheno_means <- (full_join(full_join(full_join(full_join(full_join
                        (full_join(summary_bb, summary_flower),summary_fruit), 
                         summary_leafout), summary_col.leaves),summary_leafdrop), 
                         summary_last_obs ))

pheno_means <- (full_join(full_join(full_join(
  full_join(full_join(full_join(full_join(full_join
         (full_join(summary_bb, summary_flower),summary_fruit), 
         summary_leafout), summary_col.leaves),summary_leafdrop), 
       summary_flower_open),summary_flower_pollen),
                                    summary_fruit_ripe),summary_fruit_drop))



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


# let's look at the last observations (obsolete)----
# some of the last_obs_mean is smaller than leafdrop_mean and we need to fix this
pheno_means$last_obs_mean <- ifelse(
  pheno_means$last_obs_mean < pheno_means$leafdrop_mean, 
  pheno_means$leafdrop_mean, pheno_means$last_obs_mean )
# Yayyy it worked!
# the ifelse function sets it that the last observation doy will equate to 
# leafdrop doy if it is smaller than the leafdrop doy, otherwise no change


# export to csv ----
write.csv(pheno_means, file="output/treespotters_pheno_means_across_5_years_updated_May_18.csv", row.names=FALSE)
write.csv(pheno_means, file="output/treespotters_pheno_means_individual_year_updated_May_18.csv", row.names=FALSE)

write.csv(pheno_means, file="output/treespotters_pheno_means_across_5_years_updated_May_27_allphases.csv", row.names=FALSE)
write.csv(pheno_means, file="output/treespotters_pheno_means_individual_year_updated_May_27_allphases_with_NAs.csv", row.names=FALSE)

# need to remove the NAs when plotting, can use this info for what data we need more of 