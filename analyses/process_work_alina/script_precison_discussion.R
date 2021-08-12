# script to see the percentage of data accepted
# August 3

# libraries
library(Cairo)
library(webshot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(svglite)

raw <- read.csv("input/status_intensity_observation_data_all_columns.csv", header = TRUE)

# select only columns we need

subraw <- subset(raw, select= c( "Observation_ID",  "ObservedBy_Person_ID",  "Observation_Date" ,
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


subraw <- subset(subraw,subraw$status != "-1" & subraw$status != "0") #66963


# how many observations of each phase there are before cleaning
unclean <- as.data.frame(table(subraw$phase))



# order doy based on groups 
subraw <- subraw   %>% 
  group_by(year, phase, tree_id) %>%
  arrange(doy, .by_group = TRUE)    # ascending




# calculate the difference
subraw <- subraw  %>% group_by(year, phase, tree_id)%>%
  mutate(difference = doy - lag(doy,default=first(doy)))

subraw <- subraw   %>% 
  group_by(year, phase, tree_id) %>%
  arrange(-doy, .by_group = TRUE)      # descending


subraw <- subraw  %>% group_by(year, phase, tree_id)%>%
  mutate(difference_reverse = lag(doy,default=first(doy))-doy)


# add number of observations
subraw <- subraw  %>% group_by(year, phase, tree_id, doy) %>% 
  mutate(obs_num = sum(status))           # 66963 obs


# filter out extremes
clean <- filter(subraw,difference < 5 |obs_num > 1)  # 48200 obs
# this gets rid of first doys observed by a single observer that are too far from the rest of observations 
clean <- clean[!(clean$difference == 0 & clean$difference_reverse >5 & clean$obs_num==1),]  #46416 obs

# update August 10 - Lizzie wants to expand the range from 5 to 10 just for the precision figure
clean <- filter(subraw,difference < 10 |obs_num > 1)  # 60971 obs
# this gets rid of first doys observed by a single observer that are too far from the rest of observations 
clean <- clean[!(clean$difference == 0 & clean$difference_reverse >10 & clean$obs_num==1),]  #59920 obs


# how many observations of each phase there are after cleaning
cleantable <- as.data.frame(table(clean$phase))


# merge two tables
precision <- merge(unclean, cleantable,
        by=c("Var1"),
        suffixes=c("unclean", "clean"), all.x=TRUE, all.y=TRUE)

# calculate percentage
precision$percent <- precision$Freqclean / precision$Frequnclean



# hmmm look at phenophase x species
# hmmm experiment....

clean$phaseXspp <- with(clean, paste(phase, common_name, sep = " "))
cleantable_spp <- as.data.frame(table(clean$phaseXspp))

subraw$phaseXspp <- with(subraw, paste(phase, common_name, sep = " "))
unclean_spp <- as.data.frame(table(subraw$phaseXspp))

# merge two tables
precision_spp <- merge(unclean_spp, cleantable_spp,
                   by=c("Var1"),
                   suffixes=c("unclean", "clean"), all.x=TRUE, all.y=TRUE)
# calculate percentage
precision_spp$percent <- precision_spp$Freqclean / precision_spp$Frequnclean

# export 
write.csv(precision, file = "output/precision_phase_August3.csv", row.names = F)
write.csv(precision_spp, file = "output/precision_phase_spp_August3.csv", row.names = F)


# plot phenophase first

# rename phenophase descriptions
precision  <- precision  %>%   # overwriting our data frame 
  mutate(pheno_refined =   # creating our new column
           case_when(Var1 == "budburst" ~ "Breaking leaf buds",
                     Var1 == "flowers" ~ "Flowers or flower buds",
                     Var1 == "leaf drop" ~ "Falling leaves",
                     Var1 == "leafout"~ "Leafout",
                     Var1 == "Pollen release (flowers)" ~ "Pollen release",
                     Var1 == "Colored leaves" ~ "Colored leaves",
                     Var1 == "Fruits" ~ "Fruits",
                     Var1 == "Increasing leaf size" ~ "Increasing leaf size",
                     Var1 == "Open flowers" ~ "Open flowers",
                     Var1 == "Ripe fruits" ~ "Ripe fruits",
                     Var1 == "Recent fruit or seed drop" ~ "Recent fruit or seed drop"))

# reorder rows
order <- c("Breaking leaf buds","Leafout","Increasing leaf size", 
           "Colored leaves","Falling leaves","Flowers or flower buds","Open flowers",
           "Pollen release","Fruits","Ripe fruits","Recent fruit or seed drop")

precision$pheno_refined <- factor(precision$pheno_refined,                                    # Change ordering manually
                                  levels = order)

precision <- precision %>%# hmmm for some reason this stopped working
  slice(match(order, pheno_refined)) # using slice and match function to reorder

precision <- precision  %>% arrange(factor("pheno_refined", levels = order))



precision$pheno_refined <- factor(precision$pheno_refined,                                    # Change ordering manually
                                  levels = order)


# percent sign 
precision <- precision %>% mutate_if(is.numeric, ~round(., 2))  # round
precision$percent <- precision$percent*100
precision$test <- "%"
precision$percent<- with(precision, paste(percent, test, sep = ""))


ggplot(precision, aes(x = pheno_refined)) +
  # geom_line(color = 6,lwd = 0.8,linetype = 1)+      
  geom_bar(aes(y = Frequnclean),colour = "#19578c",fill = "#19578c",stat="identity",position = "identity")+ 
  geom_text(aes(y = Frequnclean, label = percent), size = 3.5, vjust = -0.75)+
  geom_bar(aes(y = Freqclean),colour = "#96c2d7",fill = "#96c2d7",stat="identity",position = "identity")+ 
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Phenophase")  +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        # axis.text.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        # legend.position = "none" ,
        plot.margin = unit(c(1,1,1,1), units = "cm"))
ggsave(file="precision_phase_expandeddayrange10.svg", width=10, height=8)




# plot phenophase x spp

precision_spp <- read.csv("output/precision_phase_spp_August3.csv",header = T)

# break apart Var1

# rename phenophase descriptions
precision_spp  <- precision_spp  %>%   # overwriting our data frame 
  mutate(pheno_refined =   # creating our new column
           case_when(Var1 == "budburst" ~ "Breaking leaf buds",
                     Var1 == "flowers" ~ "Flowers or flower buds",
                     Var1 == "leaf drop" ~ "Falling leaves",
                     Var1 == "leafout"~ "Leafout",
                     Var1 == "Pollen release (flowers)" ~ "Pollen release",
                     Var1 == "Colored leaves" ~ "Colored leaves",
                     Var1 == "Fruits" ~ "Fruits",
                     Var1 == "Increasing leaf size" ~ "Increasing leaf size",
                     Var1 == "Open flowers" ~ "Open flowers",
                     Var1 == "Ripe fruits" ~ "Ripe fruits",
                     Var1 == "Recent fruit or seed drop" ~ "Recent fruit or seed drop"))

# reorder rows
order <- c("Breaking leaf buds","Leafout","Increasing leaf size", 
           "Colored leaves","Falling leaves","Flowers or flower buds","Open flowers",
           "Pollen release","Fruits","Ripe fruits","Recent fruit or seed drop")

precision_spp$pheno_refined <- factor(precision_spp$pheno_refined,                                    # Change ordering manually
                                  levels = order)

precision_spp <- precision_spp %>%# hmmm for some reason this stopped working
  slice(match(order, pheno_refined)) # using slice and match function to reorder

precision_spp <- precision_spp  %>% arrange(factor("pheno_refined", levels = order))



precision_spp$pheno_refined <- factor(precision_spp$pheno_refined,                                    # Change ordering manually
                                  levels = order)


# percent sign 
precision_spp <- precision_spp %>% mutate_if(is.numeric, ~round(., 2))  # round
precision_spp$percent <- precision_spp$percent*100
precision_spp$test <- "%"
precision_spp$percent<- with(precision_spp, paste(percent, test, sep = ""))



# leafout map
clean_leafout <- filter(summ_clean, phase == "leafout" & year == 2019) # the most data

# join coordinates
coordinates <- read.csv("output/table_for_gis_mapping_June_2.csv",header = T)

clean_leafout <- full_join(clean_leafout, coordinates)

# export
write.csv(clean_leafout, file = "output/table_gis_August3.csv", row.names = F)