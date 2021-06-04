# script to test if there's a relationship between the number of plant individuals 
# we have for each species and the amount of observations we have for that species

# June-4, 2021
# alina.zeng(at)ubc.ca

# I want to test if some routes had more observations because there were more trees
# or is it just that people visited those species more frequently

# libraries
library(dplyr)
library(tidyr)

# import data

# wrong data set -> should use the yes/no/unsure one
data <- read.csv("output/observation_individual_trees_June_2.csv", header = T)

data2 <- data %>%  group_by(Scientific_Names) %>% summarise(obs_spp=sum(indiv_obs.),num_indiv=length(id))