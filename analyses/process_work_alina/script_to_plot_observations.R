# Script to plot numbers of observations
# June-1, 2021
# alina.zeng(at)ubc.ca

# libraries
library(Cairo)
library(webshot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)


# plot number of observations of each phenophase ----

# read data
obs_pheno <- read.csv("output/observation_pheno_May27.csv", header = T)

# rename phenophase descriptions
obs_pheno  <- obs_pheno  %>%   # overwriting our data frame 
  mutate(pheno_refined =   # creating our new column
           case_when(Phenophase_Description == "budburst" ~ "Breaking leaf buds",
                     Phenophase_Description == "flowers" ~ "Flowers or flower buds",
                     Phenophase_Description == "leaf drop" ~ "Falling leaves",
                     Phenophase_Description == "leafout"~ "Leafout",
                     Phenophase_Description == "Pollen release (flowers)" ~ "Pollen release",
                     Phenophase_Description == "Colored leaves" ~ "Colored leaves",
                       Phenophase_Description == "Fruits" ~ "Fruits",
                       Phenophase_Description == "Increasing leaf size" ~ "Increasing leaf size",
                       Phenophase_Description == "Open flowers" ~ "Open flowers",
                      Phenophase_Description == "Ripe fruits" ~ "Ripe fruits",
                       Phenophase_Description == "Recent fruit or seed drop" ~ "Recent fruit or seed drop"))


# reorder rows
order <- c("Breaking leaf buds","Leafout","Increasing leaf size", 
           "Colored leaves","Falling leaves","Flowers or flower buds","Open flowers",
           "Pollen release","Fruits","Ripe fruits","Recent fruit or seed drop")

obs_pheno$pheno_refined <- factor(obs_pheno$pheno_refined,                                    # Change ordering manually
                                  levels = order)

obs_pheno <- obs_pheno %>%# hmmm for some reason this stopped working
  slice(match(order, pheno_refined)) # using slice and match function to reorder

obs_pheno <- obs_pheno  %>% arrange(factor("pheno_refined", levels = order))


                                             
obs_pheno$pheno_refined <- factor(obs_pheno$pheno_refined,                                    # Change ordering manually
                  levels = order)


# make bar plot
png(filename="observations_of_each_phenophase.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_pheno, aes(x = pheno_refined, y = pheno_obs.)) +
  geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#FFA500") +
  geom_hline(aes(yintercept = mean(pheno_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=1, show.legend =T) +           # Changing the look of the line                      
  theme_bw() +
  ylab("Number of observations\n") +                             
  xlab("Phenophase")  +
  coord_cartesian(ylim = c(0, 2000))+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  labs(title = "Number of Observations for Each Phenophase",
     #  caption = "placeholder",
     subtitle = "Mean number of observations in dash purple line")
         
dev.off()


# using beautiful colors
png(filename="observations_of_each_phenophase.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_pheno, aes(x = pheno_refined, y = pheno_obs., colour = pheno_refined, fill = pheno_refined)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=1, show.legend = F) +           # Changing the look of the line                      
  theme_bw() +
  scale_fill_brewer(palette = "PiYG", direction = -1)+   # reverse the order
  scale_color_brewer(palette = "PiYG", direction = -1)+  
  ylab("Number of observations\n") +                             
  xlab("Phenophase")  +
  coord_cartesian(ylim = c(0, 2000))+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  labs(title = "Number of Observations for Each Phenophase",
       #  caption = "placeholder",
       subtitle = "Mean number of observations in dash purple line")

dev.off()


# plot number of observations of each phenophase on each species ----

# read data
spp_obs_pheno <- read.csv("output/observation_pheno_spp_update_May_27.csv", header = T)

# rename phenophase descriptions
spp_obs_pheno  <- spp_obs_pheno  %>%   # overwriting our data frame 
  mutate(pheno_refined =   # creating our new column
           case_when(Phenophase_Description == "budburst" ~ "Breaking leaf buds",
                     Phenophase_Description == "flowers" ~ "Flowers or flower buds",
                     Phenophase_Description == "leaf drop" ~ "Falling leaves",
                     Phenophase_Description == "leafout"~ "Leafout",
                     Phenophase_Description == "Pollen release (flowers)" ~ "Pollen release",
                     Phenophase_Description == "Colored leaves" ~ "Colored leaves",
                     Phenophase_Description == "Fruits" ~ "Fruits",
                     Phenophase_Description == "Increasing leaf size" ~ "Increasing leaf size",
                     Phenophase_Description == "Open flowers" ~ "Open flowers",
                     Phenophase_Description == "Ripe fruits" ~ "Ripe fruits",
                     Phenophase_Description == "Recent fruit or seed drop" ~ "Recent fruit or seed drop"))

# reorder using a different method
spp_obs_pheno$pheno_refined <- factor(spp_obs_pheno$pheno_refined,     # yayy it worked                               # Change ordering manually
                                  levels = order)
spp_obs_pheno <- spp_obs_pheno  %>% arrange(factor(pheno_refined, levels = order))


# version 1: species as x axis, and phenophase as facets
png(filename="observations_of_each_phenophase_on_each_species_version1.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(spp_obs_pheno, aes(x = Scientific_Names, y = pheno_spp_obs., color = Scientific_Names), fill = Scientific_Names) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_spp_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Species")  +
  facet_wrap(~ pheno_refined, scales = "free_y") +   
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  guides(col=guide_legend("Species")) + # Set legend title and labels with a scale function
    labs(title = "Number of Observations for Each Phenophase on Each Species",
  caption = "Species as x-axis",
  subtitle = "Mean number of observations in dash purple line")
dev.off()

# same scale ----
png(filename="observations_of_each_phenophase_on_each_species_version1_scaled.png",
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(spp_obs_pheno, aes(x = Scientific_Names, y = pheno_spp_obs., color = Scientific_Names), fill = Scientific_Names) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_spp_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Species")  +
  facet_wrap(~ pheno_refined, scales = "free_y") +   
  coord_cartesian(ylim = c(0, 350))+
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  guides(col=guide_legend("Species")) + # Set legend title and labels with a scale function
  labs(title = "Number of Observations for Each Phenophase on Each Species",
       caption = "Species as x-axis",
       subtitle = "Mean number of observations in dash purple line")
dev.off()

# version 2: phenophase as x axis, and species as facets
png(filename="observations_of_each_phenophase_on_each_species_version2.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(spp_obs_pheno, aes(x = pheno_refined, y = pheno_spp_obs., color = Scientific_Names), fill = Scientific_Names) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_spp_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Phenophase")  +
  facet_wrap(~ Scientific_Names, scales = "free_y") +   
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  guides(col=guide_legend("Species")) + # Set legend title and labels with a scale function
  labs(title = "Number of Observations for Each Phenophase on Each Species",
  caption = "Phenophase as x-axis",
  subtitle = "Mean number of observations in dash purple line")
dev.off()

# same scale ----
png(filename="observations_of_each_phenophase_on_each_species_version2_scaled.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(spp_obs_pheno, aes(x = pheno_refined, y = pheno_spp_obs., color = Scientific_Names), fill = Scientific_Names) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_spp_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Phenophase")  +
  facet_wrap(~ Scientific_Names) +   
  coord_cartesian(ylim = c(0, 350))+
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  guides(col=guide_legend("Species")) + # Set legend title and labels with a scale function
  labs(title = "Number of Observations for Each Phenophase",
caption = "Phenophase as x-axis; y-values in this graph are fixed",
subtitle = "Mean number of observations in dash purple line")
dev.off()


# plot observations by route

# read data
obs_route <- read.csv("output/observation_routes_May27.csv", header = T)


# make bar plot
png(filename="observations_of_each_route.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_route, aes(x = Route_Name, y = route_obs., fill = Route_Name)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(route_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=1, show.legend = F) +           # Changing the look of the line                      
  scale_fill_brewer(palette = "Set3")+  
  theme_bw() +
  ylab("Number of observations\n") +                             
  xlab("Route name")  +
  coord_cartesian(ylim = c(0, 4200))+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none" ,
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  labs(title = "Number of Observations on Each Route",
       #  caption = "placeholder",
       subtitle = "Mean number of observations in dash purple line")

dev.off()