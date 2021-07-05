## script to plot Yes & No & Unsure (facet by phenophase)


# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(Cairo)
library(webshot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)


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
bb.pheno <- dplyr::select(bb, Common_Name, phase, 
                           Phenophase_Status)
## if bb.pheno$phase=="Breaking leaf buds", change it to "budburst", otherwise keep it as it is
## the same goes for the codes below
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)




# figure out which ones are valid, which ones are not (including phenophase)
# 1 ["yes the phenophase is occuring"]|0 ["no the phenophase is not occuring"]
# |-1 ["not certain whether the phenophase is occuring"]
pheno_grouped <- bb.pheno %>% group_by(phase,Phenophase_Status )
occurence <- tally(pheno_grouped)
occurence <- occurence %>% mutate(status = 
                                    case_when(Phenophase_Status  == "1" ~ "Yes, the phenophase is occuring",
                                              Phenophase_Status  == "0" ~ "No, the phenophase is not occuring",
                                              Phenophase_Status  == "-1" ~ "Unsure"))

# rename columns
occurence <- rename(occurence, "frequency" = "n")



# modify phases
occurence  <- occurence  %>%   # overwriting our data frame 
  mutate(pheno_refined =   # creating our new column
           case_when(phase == "budburst" ~ "Breaking leaf buds",
                     phase == "flowers" ~ "Flowers or flower buds",
                     phase == "leaf drop" ~ "Falling leaves",
                     phase == "leafout"~ "Leaves",
                     phase == "Pollen release (flowers)" ~ "Pollen release",
                     phase == "Colored leaves" ~ "Colored leaves",
                     phase == "Fruits" ~ "Fruits",
                     phase == "Increasing leaf size" ~ "Increasing leaf size",
                     phase == "Open flowers" ~ "Open flowers",
                     phase == "Ripe fruits" ~ "Ripe fruits",
                     phase == "Recent fruit or seed drop" ~ "Recent fruit or seed drop"))

# reorder rows
order <- c("Breaking leaf buds","Leaves","Increasing leaf size", 
           "Colored leaves","Falling leaves","Flowers or flower buds","Open flowers",
           "Pollen release","Fruits","Ripe fruits","Recent fruit or seed drop")

occurence$pheno_refined <- factor(occurence$pheno_refined,     # yayy it worked                               # Change ordering manually
                                      levels = order)



# make bar plot
png(filename="occurence_of_phenophase_status_facet_phenophase.png", 
    type="cairo", 
    units="in", 
    width=12, 
    height=8, 
    res=300)
ggplot(occurence, aes(x = status, y = frequency, fill = status)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_text(aes(label = frequency), size = 3, vjust = -0.75)+
  scale_fill_manual(values = c("#a8d13d","#caa518","#d67829"))+  
  theme_classic() +
  ylab("Number of occurence\n") +                             
  xlab("\n Status")  +
  facet_wrap(~ pheno_refined, scales = "fixed") +   
  coord_cartesian(ylim = c(0, 29000))+
  theme(# axis.text.x = element_text(size = 5, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        # axis.text.y = element_text(size = 12),
        axis.text.y=element_blank(),  # hide y axis label,
        axis.text.x=element_blank(),  # hide x axis label
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
       legend.position = "none" ,
        plot.margin = unit(c(0.5,1,1,1), units = "cm"))
  # guides(fill=guide_legend("Status"), size = 12, face = "bold") +
  # labs(title = "Number of Yes & No & Unsure grouped by Phenophases")
       #  caption = "placeholder",
      # subtitle = "Out of 334180 observations, 66963 of them document observation of a phenophase occuring")
# dev.off()

ggsave(file="observations_yes_no_unsure.svg", width=10, height=10)