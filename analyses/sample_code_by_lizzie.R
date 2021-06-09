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
cl <- read.csv("output/clean_bb_leafout_data_June_9.csv", header =  T)
uncl <- read.csv("output/unclean_bb_leafout_data_June_9.csv", header =  T)

# Side note -- when coming up with names, best to do spaces or capitals, 
# not both (e.g., Scientific_Names) and honestly simpler is sometimes better, 
# I would go with "species' or "latbi" (for Latin binomial)


# get the summary we need for plotting ... 
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


# Here's all species and years for one phase ... 
png(filename="option1_spp_as_facet.png", 
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

# Here's one species so can see data better, for one phase ... 
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

# Here's all species and years, but the other way, for one phase ... 
png(filename="option3_year_as_facet.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(subset(dat, phase=="budburst"),
       aes(quant5clean, quant5unclean, color=Scientific_Names)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + 
  facet_wrap(year~., scales="free")
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



png(filename="option1_spp_as_facet_no_id.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(subset(dat2, phase=="budburst"),
       aes(quant5clean, quant5unclean, group=year, color=year)) +
  geom_point() +
  scale_color_brewer(palette = "Paired")+  
  geom_abline(intercept = 0, slope = 1) + 
  facet_wrap(Scientific_Names~., scales="free")
dev.off()




