# script to document number of Yes and No observations on each species 
# June-4, 2021
# just realized I should do # of observations on species using non-subset data


# libraries
library(dplyr)
library(tidyr)
library(Cairo)
library(ggplot2)
library(RColorBrewer)

# import data

data <- read.csv(file = "output/observation_indivisual_trees_June_4_YES_No.csv", header = T)

# join the genus and the species columns ----
data$Scientific_Names <- with(data, paste(genus, species, sep = " "))


# count the number of observations made on individual trees and shrubs ----
indiv_obs  <- data %>%
  group_by(id) %>%
  summarise("indiv_obs" = length(id))

# count the number of observations made on each species
spp_obs <- data %>%
  group_by(Scientific_Names) %>%
  summarise("spp_obs" = length(Scientific_Names))

# by year
spp_obs_year <- data %>%
  group_by(Scientific_Names, year) %>%
  summarise("spp_obs_year" = length(Scientific_Names))

# plot total ----
png(filename="observations_of_each_spp_June_4.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=10, 
    res=300)
ggplot(spp_obs, aes(x = Scientific_Names, y = spp_obs, colour = Scientific_Names, fill = Scientific_Names)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(spp_obs)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.75, show.legend = F) +           # Changing the look of the line                      
  geom_text(aes(label = spp_obs), size = 3.5, vjust = -0.75, fontface = "bold")+
  theme_classic() +
  ylab("Number of total observations\n") +                             
  xlab("Species")  +
  coord_cartesian(ylim = c(0, 42000))+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  labs(title = "Number of Observations for Each Species",
       #  caption = "placeholder",
       subtitle = "Mean number of observations in dash purple line")
dev.off()


mean(spp_obs$spp_obs)=22278.67


# plot by year
# year as x=axis, species as facets

png(filename="observations_of_species_by_year.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(spp_obs_year, aes(x = year, y = spp_obs_year, color = Scientific_Names, fill = Scientific_Names)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(spp_obs_year)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Year")  +
  facet_wrap(~ Scientific_Names, scales = "free_y") +   
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 9, face = "italic") )+  # changing the fontface for legend labels
  guides(fill=guide_legend("Species"), size = 12, face = "bold") +
  guides(col=guide_legend("Species"),size = 12, face = "bold") +# Set legend title and labels with a scale function
  labs(title = "Number of Observations for Each Species Each Year",
      # caption = "Phenophase as x-axis",
       subtitle = "Mean number of observations in dash purple line")
dev.off()


# plot indiv ----
# join by Scientific names and keep only the fields we need
indiv_obs <- select(full_join(indiv_obs,data), c(id, indiv_obs, route, Scientific_Names))
indiv_obs <- unique(indiv_obs)

# change id to characters
indiv_obs$id <- as.character(indiv_obs$id)

# Individual ID as x=axis, species as facets

png(filename="observations_of_individuals.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(indiv_obs, aes(x = id, y = indiv_obs, color = Scientific_Names, fill = Scientific_Names)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(indiv_obs)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Individual ID")  +
  facet_wrap(~ Scientific_Names, scales = "fixed") +   
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 9, face = "italic") )+  # changing the fontface for legend labels
  guides(fill=guide_legend("Species"), size = 12, face = "bold") +
  guides(col=guide_legend("Species"),size = 12, face = "bold") +# Set legend title and labels with a scale function
  labs(title = "Number of Observations for Each Individual Grouped by Species",
       caption = "This is a terrible graph, will fix later",
       subtitle = "Mean number of observations in dash purple line")
dev.off()


# Individual ID as x=axis, route as facets

png(filename="observations_of_individuals_route_facet.png", 
    type="cairo", 
    units="in", 
    width=14, 
    height=10, 
    res=300)
ggplot(indiv_obs, aes(x = id, y = indiv_obs, color = Scientific_Names, fill = Scientific_Names)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(indiv_obs)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=0.5, show.legend = F) +
  theme_classic() +
  ylab("Number of observations\n") +                             
  xlab("\n Individual ID")  +
  facet_wrap(~ route, scales = "fixed") +   
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 9, face = "italic") )+  # changing the fontface for legend labels
  guides(fill=guide_legend("Species"), size = 12, face = "bold") +
  guides(col=guide_legend("Species"),size = 12, face = "bold") +# Set legend title and labels with a scale function
  labs(title = "Number of Observations for Each Individual Grouped by Routes",
       caption = "This is a terrible graph, will fix later",
       subtitle = "Mean number of observations in dash purple line")
dev.off()




# route ----

# count the number of observations made along each route ----
route_obs <- data %>%
  group_by(route) %>%
  summarise("route_obs" = length(route))

png(filename="observations_of_each_route.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(route_obs, aes(x = reorder(route,route_obs, y = route_obs, fill = route)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(route_obs)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=1, show.legend = F) +           # Changing the look of the line                      
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

dev.off()




# linear model

route_obs <- indiv_obs %>%
  group_by(route) %>%
  summarise(obs_route=sum(indiv_obs),num_indiv=length(id))


route_obs$num_indiv <- as.factor(route_obs$num_indiv )
library(ggplot2)

png(filename="observations_of_each_route_linear_model.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(route_obs, aes(num_indiv, obs_route)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
   # coord_cartesian(xlim = c(7,11))+ #muting shrub route
    theme_classic() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Number of Trees per Route", y = "Number of Observation per Route")
dev.off()

# hmmm getting rid of shrub route cuz it was newly added...
route.m <- lm( obs_route ~ num_indiv, data = route_obs[1:7,])
summary(route.m)
# export

# hmmm i will think about what to export later
