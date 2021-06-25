# Codes from Lizzie
# June-9, 2021

rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Desktop")

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Cairo)

# import data
cl <- read.csv("output/clean_raw_data_June_9.csv", header =  T)
uncl <- read.csv("output/unclean_raw_data_June_9.csv", header =  T)

# Side note -- when coming up with names, best to do spaces or capitals, 
# not both (e.g., Scientific_Names) and honestly simpler is sometimes better, 
# I would go with "species' or "latbi" (for Latin binomial)


# get the summary we need for plotting ---- 
datsummcl <-
  ddply(cl, c("Scientific_Names", "phase", "year", "id"),
        summarise,
        quant5 = quantile(doy, 0.05, na.rm=TRUE),
        quant50 = quantile(doy, 0.5, na.rm=TRUE),
        quant95 = quantile(doy, 0.95, na.rm=TRUE),
  )

datsummuncl <-
  ddply(uncl, c("Scientific_Names", "phase", "year", "id"),
        summarise,
        quant5 = quantile(doy, 0.05, na.rm=TRUE),
        quant50 = quantile(doy, 0.5, na.rm=TRUE),
        quant95 = quantile(doy, 0.95, na.rm=TRUE),
  )

# merge so we can plot what we want ...
dat <- merge(datsummcl, datsummuncl,
             by=c("Scientific_Names", "phase", "year", "id"),
             suffixes=c("clean", "unclean"), all.x=TRUE, all.y=TRUE)

# plot, but not sure the best option ...


dat$year <- as.factor(dat$year)


# Here's all species and years for one phase ----
png(filename="option1_budburst_5.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(subset(dat, phase=="budburst"),
       aes(quant5clean, quant5unclean, group=year, color=year)) +
  geom_point() +
  theme_classic()+
  scale_color_brewer(palette = "Paired")+  
  geom_abline(intercept = 0, slope = 1) + 
  facet_wrap(Scientific_Names~., scales="free")+
  guides(col=guide_legend("Year")) + 
  labs(  x = "\n 5% Quantile Clean", 
    y = "5% Quantile Unclean \n",
    title = "Budburst",
    #  caption = "placeholder")
    subtitle = "5% Quantile")
dev.off()

# Here's one species so can see data better, for one phase ----
png(filename="option2_one_spp.png", 
    type="cairo", 
    units="in", 
    width=9, 
    height=3, 
    res=300)
ggplot(subset(dat, phase=="budburst" & Scientific_Names=="Acer rubrum"),
       aes(quant5clean, quant5unclean)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + 
  facet_wrap(year~., scales="free")+
labs( # x = "\n Year", 
     # y = "Number of observation contributed by individual Tree Spotters \n",
     title = "Acer rubrum",
#  caption = "placeholder")
     subtitle = "placeholder")
dev.off()

# Here's all species and years, but the other way, for one phase ----
png(filename="option3_fruit_seed_drop_5.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(subset(dat, phase=="Recent fruit or seed drop"),
       aes(quant5clean, quant5unclean, color=Scientific_Names)) +
  geom_point() +
  guides(col=guide_legend("Species")) + 
  theme_minimal()+
  geom_abline(intercept = 0, slope = 1) + 
  facet_wrap(year~., scales="free")+
  theme(legend.text = element_text(size = 7.5, face = "italic") )+
  labs(  x = "\n 5% Quantile Clean", 
         y = "5% Quantile Unclean \n",
         title = "Recent fruit or seed drop",
         #  caption = "placeholder")
         subtitle = "5% Quantile")
dev.off()
# hmm, we should check why 2015 and 2018 have such high y axis values

# drop "id" ----
datsummcl2 <-
  ddply(cl, c("Scientific_Names", "phase", "year"),
        summarise,
        quant5 = quantile(doy, 0.05, na.rm=TRUE),
        quant50 = quantile(doy, 0.5, na.rm=TRUE),
        quant95 = quantile(doy, 0.95, na.rm=TRUE),
  )

datsummuncl2 <-
  ddply(uncl, c("Scientific_Names", "phase", "year"),
        summarise,
        quant5 = quantile(doy, 0.05, na.rm=TRUE),
        quant50 = quantile(doy, 0.5, na.rm=TRUE),
        quant95 = quantile(doy, 0.95, na.rm=TRUE),
  )

# merge so we can plot what we want ...
dat2 <- merge(datsummcl2, datsummuncl2,
             by=c("Scientific_Names", "phase", "year"),
             suffixes=c("clean", "unclean"), all.x=TRUE, all.y=TRUE)

dat2$year <- as.factor(dat2$year)

png(filename="option1_budburst_5_no_id.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(subset(dat2, phase=="budburst"),
       aes(quant5clean, quant5unclean, group=year, color=year)) +
  geom_point() +
  theme_classic()+
  scale_color_brewer(palette = "Paired")+  
  geom_abline(intercept = 0, slope = 1) + 
  facet_wrap(Scientific_Names~., scales="free")+
  guides(col=guide_legend("Year")) + 
  labs(  x = "\n 5% Quantile Clean", 
         y = "5% Quantile Unclean \n",
         title = "Budburst",
         #  caption = "placeholder")
         subtitle = "5% Quantile")
dev.off()



# update on June 24, 2021
# filtering out weird data

# Acer rubrum
id = c(166764, 166765)
aceroutliers_cl <-subset(cl, cl$id %in% c( "166764","166765" ))
aceroutliers_uncl <-subset(uncl, uncl$id %in% c( "166764","166765" ))


# order stages

order <- c("Breaking leaf buds","Leaves","Increasing leaf size", 
           "Colored leaves","Falling leaves","Flowers or flower buds","Open flowers",
           "Pollen release","Fruits","Ripe fruits","Recent fruit or seed drop")

aceroutliers_cl  <- aceroutliers_cl  %>%   # overwriting our data frame 
  mutate(phase_refined =   # creating our new column
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

aceroutliers_uncl  <- aceroutliers_uncl  %>%   # overwriting our data frame 
  mutate(phase_refined =   # creating our new column
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
aceroutliers_cl$phase_refined <- factor(aceroutliers_cl$phase_refined,     # yayy it worked                               # Change ordering manually
                                      levels = order)
aceroutliers_cl <- aceroutliers_cl  %>% arrange(factor(phase_refined, levels = order))


aceroutliers_uncl$phase_refined <- factor(aceroutliers_uncl$phase_refined,     # yayy it worked                               # Change ordering manually
                                        levels = order)
aceroutliers_uncl <- aceroutliers_uncl  %>% arrange(factor(phase_refined, levels = order))


# export for lizzie to have a look

write.csv(aceroutliers_cl, file = "output/aceroutliers_cl_June24.csv", row.names = F)
write.csv(aceroutliers_uncl, file = "output/aceroutliers_uncl_June24.csv", row.names = F)
