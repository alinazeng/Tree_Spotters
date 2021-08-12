# June-5, 2021
# Today I create final svgs for TS document
# alina.zeng@ubc.ca

# libraries ---- 
library(Cairo)
library(webshot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(svglite)

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
                                  levels = order)   # works here
obs_pheno <- obs_pheno  %>% arrange(factor("pheno_refined", levels = order))
obs_pheno$pheno_refined <- factor(obs_pheno$pheno_refined,                                    # Change ordering manually
                                  levels = order)

# using beautiful colors # doesnt really matter cuz I will be using colorpicker
png(filename="observations_of_each_phenophase_pretty.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_pheno, aes(x = pheno_refined, y = pheno_obs., colour = pheno_refined, fill = pheno_refined)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_obs.)),       # Adding a line for mean observation
             colour = "#627a8d", linetype = "dashed", size=0.75, show.legend = F) +           # Changing the look of the line                      
  theme_minimal() +
  scale_fill_manual(values = c("#a9d03d",
                               "#b1c832",
                               "#b9bf28",
                               "#bfb620",
                               "#c5ad1b",
                               "#c9a418",
                               "#cd9b18",
                               "#d0921a",
                               "#d3891e",
                               "#d48023",
                               "#d57728")) +                # Adding custom colours for solid geoms (ribbon)
  scale_colour_manual(values = c("#a9d03d",
                                 "#b1c832",
                                 "#b9bf28",
                                 "#bfb620",
                                 "#c5ad1b",
                                 "#c9a418",
                                 "#cd9b18",
                                 "#d0921a",
                                 "#d3891e",
                                 "#d48023",
                                 "#d57728"))+                # Adding custom colours for lines and points
  ylab("Number of observations\n") +                             
  xlab("Phenophase")  +
  coord_cartesian(ylim = c(0, 21000))+
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
# labs(title = "Number of Observations for Each Phenophase",
#  caption = "placeholder",
#  subtitle = "Mean number of observations in purple dash line")
# dev.off()
ggsave(file="observations_of_each_phenophase.svg", width=10, height=8)



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

# same scale ----
png(filename="observations_of_each_phenophase_on_each_species_July26.png", 
    type="cairo", 
    units="in", 
    width=10, 
    height=10, 
    res=300)
ggplot(spp_obs_pheno, aes(x = pheno_refined, y = pheno_spp_obs.,fill = Scientific_Names)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_spp_obs.)),       # Adding a line for mean observation
             colour = "#627a8d", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  scale_color_manual(values = "#627a8d")+
  scale_fill_manual(values = c("#345C24","#8DBC73","#4CA38B","#D0E7DF","#DBEB5C","#BAD77C","#EBDB44",
                               "#7DB376","#E6F597","#5B7C4C","#A6A939","#DBD34E","#DBEB7B","#CCC43C","#ECDC4C")) +                # Adding custom colours for solid geoms (ribbon)
  ylab("Number of observations\n") +                             
  xlab("\n Phenophase")  +
  facet_wrap(~ Scientific_Names, scales = "fixed") +   
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"), 
        legend.position = "none",
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = "cm"))+
  guides(col=guide_legend("Species"))  # Set legend title and labels with a scale function
# labs(title = "Number of Observations for Each Phenophase on Each Species")
# dev.off()
ggsave(file="observations_of_each_phenophase_on_each_spp.svg", width=12, height=12)




# plot observations by route ----

# read data
obs_route <- read.csv("output/observation_routes_June_2.csv", header = T)

# To order by category in ggplot, first make a new factor variable

obs_route <- obs_route%>% arrange(desc(obs_route$route_obs.))

obs_route$route_obs_ordered <- factor(obs_route$route_obs., 
                               levels = names(sort(table(obs_route$route_obs.), decreasing = TRUE)) )



# make bar plot
png(filename="observations_of_each_route_orangeyellow.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_route, aes(x = reorder(route,-route_obs.), y = route_obs., fill = route)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(route_obs.)),       # Adding a line for mean observation
             colour = "#627a8d", linetype = "dashed", size=1, show.legend = F) +           # Changing the look of the line                      
  scale_fill_manual(values = c("#ff4518",
                               "#ff6500",
                               "#ff8200",
                               "#ff9c00",
                               "#ffb600",
                               "#ffcf00",
                               "#ffe700",
                               "#ffff2d"))+  
  theme_bw() +
  ylab("Number of observations\n") +                             
  xlab("Route name")  +
  coord_cartesian(ylim = c(0, 18000))+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none" ,
        plot.margin = unit(c(1,1,1,1), units = "cm"))
# dev.off()


png(filename="observations_of_each_route.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(route_obs, aes(x = reorder(route,-route_obs), y = route_obs, fill = route)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(route_obs)),       # Adding a line for mean observation
             colour = "#627a8d", linetype = "dashed", size=1, show.legend = F) +           # Changing the look of the line                      
  scale_fill_brewer(palette = "Set3")+  
  theme_bw() +
  ylab("Number of observations\n") +                             
  xlab("Route name")  +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none" ,
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
ggsave(file="observations_route.svg", width=10, height=8)


observer_year <- read.csv("output/number_of_observation_by_each_individual_each_year_June_2.csv", header=T)
observer_year$observerID <- as.character(observer_year$observerID)
observer_total <- select(observer_year,c(observerID,observation_total))
observer_total <- unique(observer_total)
observer_year$year <- as.factor(observer_year$year)


png(filename="number_of_observations_each_year_compare_to_top20_bar.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(top20observation_year , aes(x = year)) +
  # geom_line(color = 6,lwd = 0.8,linetype = 1)+      
  geom_bar(aes(y = total),colour = "#8BCAAB",fill = "#8BCAAB",stat="identity",position = "identity")+ 
  geom_text(aes(y= total, label = total), size = 5, vjust = -0.75)+
  geom_bar(aes(y = top20),colour = "#61A7BB",fill = "#61A7BB",stat="identity",position = "identity")+ 
  geom_text(aes(y= top20, label = top20), size = 5, vjust = 1.5)+
  geom_smooth(aes(y = total), size = 0.5 , color = "#8B8378")+ 
  theme_bw() +
  ylab("Number of observations\n") +                             
  xlab("\n Year")  +
  theme(axis.text.x = element_text(size = 14,vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        # axis.text.y = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        # legend.position = "none" ,
        plot.margin = unit(c(1,1,1,1), units = "cm"))
  #labs(title = "Number of Total Observations Each Year",
       #  caption = "placeholder",
      # subtitle = "Portion in orange represents contribution made by top 20% Tree Spotters ")
#dev.off()
ggsave(file="observations_individual_contribution_year.svg", width=12, height=8)

# make boxplot ----
observer_year$year <- as.factor(observer_year$year)

png(filename="observations_of_each_indiv_across_years_boxplot.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(observer_year, aes(year, observation_year)) +
  geom_boxplot(fill = "#8BCAAB", alpha = 0.8, colour = "#61A7BB") +
  theme_bw() +  
  theme(axis.text.x = element_text(size = 14, angle = 0),
        axis.text.y = element_text(size = 14, angle = 0)) +
 labs(x = "\n Year", 
       y = "Number of observation contributed by individual Tree Spotters \n")
     #  title = "Contribution of Individual Tree Spotters over 2015-2020")
#  caption = "Bars listed in ascending order")
# subtitle = "Out of 334180 observations, 66963 of them document observation of a phenophase occuring")
ggsave(file="observations_individual_contribution_boxplot.svg", width=12, height=8)



png(filename="observations_of_each_indiv_across_years_boxplot.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(observer_year, aes(year, observation_year)) +
  geom_boxplot(fill = "#d47826", alpha = 0.8, colour = "#2b4a82") +
  theme_bw() +  
  theme(axis.text.x = element_text(size = 14, angle = 0)) +
  labs(x = "\n Year", 
       y = "Number of observation contributed by individual Tree Spotters \n")

ggsave(file="observations_individual_contribution_boxplot.svg", width=10, height=8)



png(filename="observations_of_each_spp_June_4.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=10, 
    res=300)
ggplot(spp_obs, aes(x = reorder(Scientific_Names,-spp_obs), y = spp_obs, fill = Scientific_Names)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(spp_obs)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.75, show.legend = F) +           # Changing the look of the line                      
  # geom_text(aes(label = spp_obs), size = 3.5, vjust = -0.75, fontface = "bold")+
  theme_bw() +
  scale_fill_manual(values = c("#345C24","#8DBC73","#4CA38B","#D0E7DF","#DBEB5C","#BAD77C","#EBDB44",
                               "#7DB376","#E6F597","#5B7C4C","#A6A939","#DBD34E","#DBEB7B","#CCC43C","#ECDC4C")) + 
  ylab("Number of total observations\n") +                             
  xlab("Species")  +
  coord_cartesian(ylim = c(0, 42000))+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
dev.off()

ggsave(file="observations_spp2.svg", width=12, height=8)

mean(spp_obs$spp_obs)=22278.67


png(filename="observations_of_species_by_year.png", 
    type="cairo", 
    units="in", 
    width=12, 
    height=10, 
    res=300)
ggplot(spp_obs_year, aes(x = year, y = spp_obs_year,fill = Scientific_Names)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(spp_obs_year)),       # Adding a line for mean observation
             colour = "#627a8d", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Year")  +
  facet_wrap(~ Scientific_Names, scales = "fixed") +   
  scale_fill_manual(values = c("#68C6BC","#DABB78","#61A7BB","#99774E","#3C271A",
                               "#424F42","#604B1D","#7C8A8E","#626951","#5F4638","#D59529","#A6A265","#99C943","#978271","#C46434")) + 
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.position="none")
ggsave(file="observations_spp_year.svg", width=10, height=10)

