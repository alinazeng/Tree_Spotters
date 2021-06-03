# update on June-2, using status intensity dataset outputs
# June-2, 2021
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
obs_pheno <- read.csv("output/observation_pheno_June_2.csv", header = T)

obs_pheno  <- obs_pheno  %>%   # overwriting our data frame 
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

obs_pheno$pheno_refined <- factor(obs_pheno$pheno_refined,                                    # Change ordering manually
                                  levels = order)
obs_pheno <- obs_pheno  %>% arrange(factor("pheno_refined", levels = order))
obs_pheno$pheno_refined <- factor(obs_pheno$pheno_refined,                                    # Change ordering manually
                                  levels = order)

# using beautiful colors
png(filename="observations_of_each_phenophase_pretty.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_pheno, aes(x = pheno_refined, y = pheno_obs., colour = pheno_refined, fill = pheno_refined)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.75, show.legend = F) +           # Changing the look of the line                      
  theme_minimal() +
  scale_fill_brewer(palette = "PiYG", direction = -1)+   # reverse the order
  scale_color_brewer(palette = "PiYG", direction = -1)+  
  ylab("Number of observations\n") +                             
  xlab("Phenophase")  +
  coord_cartesian(ylim = c(0, 21000))+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  labs(title = "Number of Observations for Each Phenophase",
       #  caption = "placeholder",
       subtitle = "Mean number of observations in purple dash line")

dev.off()

# mean(obs_pheno$pheno_obs.)= 6087.545
# mean(spp_obs_pheno$pheno_spp_obs.)= 418.5188
# mean(obs_route$route_obs.)= 8730.375

# plot number of observations of each phenophase on each species ----

# read data
spp_obs_pheno <- read.csv("output/observation_pheno_spp_update_June_2.csv", header = T)

# modify phases
spp_obs_pheno  <- spp_obs_pheno  %>%   # overwriting our data frame 
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
spp_obs_pheno$pheno_refined <- factor(spp_obs_pheno$pheno_refined,     # yayy it worked                               # Change ordering manually
                                      levels = order)
spp_obs_pheno <- spp_obs_pheno  %>% arrange(factor(pheno_refined, levels = order))

# add scientific names
names <- read.csv("input/names.csv", header = T)
spp_obs_pheno <- full_join(spp_obs_pheno, names)

# version 1: species as x axis, and phenophase as facets
png(filename="observations_of_each_phenophase_on_each_species_version1_June2.png", 
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
  coord_cartesian(ylim = c(0, 2500))+
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
# version 2: phenophase as x axis, and species as facets ----
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
  coord_cartesian(ylim = c(0, 2200))+
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


# plot observations by route ----

# read data
obs_route <- read.csv("output/observation_routes_June_2.csv", header = T)


# make bar plot
png(filename="observations_of_each_route.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_route, aes(x = route, y = route_obs., fill = route)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(route_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=1, show.legend = F) +           # Changing the look of the line                      
  scale_fill_brewer(palette = "Set3")+  
  theme_bw() +
  ylab("Number of observations\n") +                             
  xlab("Route name")  +
  coord_cartesian(ylim = c(0, 18000))+
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

# plot phenophase status (1,0,-1) ----
occurence <- read.csv("output/occurence_of_observation_status_June_2.csv", header = T)

# make bar plot
png(filename="occurence_of_phenophase_status.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=8, 
    res=300)
ggplot(occurence, aes(x = status, y = frequency, fill = status)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_text(aes(label = frequency), size = 3.5, vjust = -0.75, fontface = "bold")+
  scale_fill_brewer(palette = "Set1")+  
  theme_minimal() +
  ylab("Number of occurence\n") +                             
  xlab("\n Status")  +
 # coord_cartesian(ylim = c(0, 260000))+
  theme(axis.text.x = element_text(size = 10),  # Angled labels, so text doesn't overlap
       # axis.text.y = element_text(size = 12),
        axis.text.y=element_blank(),  # hide y axis label
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none" ,
        plot.margin = unit(c(0.5,1,1,1), units = "cm"))+
  labs(title = "Number of Yes & No & Unsure",
       #  caption = "placeholder",
       subtitle = "Out of 334180 observations, 66963 of them document observation of a phenophase occuring")
dev.off()

# plot number of observations across year ----
observation_year <- read.csv("output/number_of_observation_each_year_June_2.csv", header=T)


# make line plot
png(filename="number_of_observations_each_year.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(observation_year, aes(x = year, y = frequency)) +
  # geom_line(color = 6,lwd = 0.8,linetype = 1)+      
  geom_smooth()+ 
  geom_text(aes(label = frequency), size = 3, vjust = -0.75)+
  theme_bw() +
  ylab("Number of observations\n") +                             
  xlab("\n Year")  +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none" ,
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  labs(title = "Number of Observations Each Year")
       #  caption = "placeholder",
      # subtitle = "Mean number of observations in dash purple line")
dev.off()


# plot individual observer across year .....

observer_year <- read.csv("output/number_of_observation_by_each_individual_each_year_June_2.csv", header=T)
observer_year <- observer_year %>% group_by(year)
boxplot <- ggplot(observer_year, aes(year, observation_year)) + geom_boxplot()

# make a plot do display difference in contribution
observer_year$observerID <- as.character(observer_year$observerID)
observer_total <- select(observer_year,c(observerID,observation_total))
observer_total <- unique(observer_total)

# gotta change how this plot looks (also rename test 2, etc)
# note in the caption that this shows over 226 tS and some contributed a lot
png(filename="number_of_contributions_by_each_individual_ordered.png", 
    type="cairo", 
    units="in", 
    width=10, 
    height=8, 
    res=300)
ggplot(observer_total, aes(reorder(observerID,observation_total), y = observation_total, fill = observerID)) +
  geom_bar(position = position_dodge(), stat = "identity") +
 # geom_text(aes(label = observation_total), size = 3.5, vjust = -0.75, fontface = "bold")+
  theme_minimal() +
  ylab("Number of observations contributed by each Tree Spotter \n") +                             
  xlab("Each bar represents one of our 226 Tree Spotters")  +
   coord_cartesian(ylim = c(0, 30500))+
  theme(# axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.text.x=element_blank(),  # hide y axis label
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none" ,
        plot.margin = unit(c(0.5,1,1,1), units = , "cm"))+
  labs(title = "Contribution of Individual Tree Spotters over 2015-2020",
        caption = "Bars listed in ascending order")
      # subtitle = "Out of 334180 observations, 66963 of them document observation of a phenophase occuring")
dev.off()


# calculate percentages of individual contribution ----
observer_total <- arrange(observer_total, observation_total)
sum(test2$observation_total[217:226])/sum(test2$observation_total)
# = 0.5121551
sum(test2$observation_total[181:226])/sum(test2$observation_total)
# = 0.8500598

# super quickly make a table documenting top20% observer's contribution by year and graph it 
top20observer <- subset(observer_year, observer_year$observation_total >= 1254)
top20observation_year <- top20observer %>% group_by(year) %>% summarise(sum = sum(observation_year))

# combine tables
top20observation_year <- full_join(top20observation_year,observation_year)
top20observation_year <- rename(top20observation_year, top20 = sum, total = frequency)

# make line plot
png(filename="number_of_observations_each_year_compare_to_top20.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(top20observation_year , aes(x = year)) +
  # geom_line(color = 6,lwd = 0.8,linetype = 1)+      
  geom_smooth(aes(y = total))+ 
  geom_text(aes(y= total, label = total), size = 3, vjust = -0.75)+
  geom_smooth(aes(y = top20))+ 
  geom_text(aes(y= top20, label = top20), size = 3, vjust = 0.75)+
  theme_bw() +
  ylab("Number of observations\n") +                             
  xlab("\n Year")  +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        # legend.position = "none" ,
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  labs(title = "Number of Observations Each Year")
#  caption = "placeholder",
# subtitle = "Mean number of observations in dash purple line")
dev.off()

# try out using bars

png(filename="number_of_observations_each_year_compare_to_top20_bar.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(top20observation_year , aes(x = year)) +
  # geom_line(color = 6,lwd = 0.8,linetype = 1)+      
  geom_bar(aes(y = total),colour = "#9A32CD",fill = "#9A32CD",stat="identity",position = "identity")+ 
  geom_text(aes(y= total, label = total), size = 3, vjust = -0.75)+
  geom_bar(aes(y = top20),colour = "#FF7F00",fill = "#FF7F00",stat="identity",position = "identity")+ 
  geom_text(aes(y= top20, label = top20), size = 3, vjust = 1.5)+
  geom_smooth(aes(y = total), size = 0.5 , color = "#8B8378")+ 
  theme_minimal() +
  ylab("Number of observations\n") +                             
  xlab("\n Year")  +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        # axis.text.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        # legend.position = "none" ,
        plot.margin = unit(c(1,1,1,1), units = "cm"))+
  labs(title = "Number of Total Observations Each Year",
#  caption = "placeholder",
 subtitle = "Portion in orange represents contribution made by top 20% Tree Spotters ")
dev.off()

