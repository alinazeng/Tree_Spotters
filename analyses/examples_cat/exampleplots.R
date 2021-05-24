## Tree Spotters example plot
# Cat - 17 May 2021

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(tidyr)
library(ggplot2)
library(viridis)

# Set Working Directory
setwd("~/Documents/git/Tree_Spotters/")

df<-read.csv("output/treespotters_pheno_means_individual_year_updated_May_18.csv",header=TRUE)

leaves <- subset(df, select=c(scientific_names, year, bb_mean, leafout_mean))
leaves <- gather(leaves, pheno, doy, -scientific_names, -year)
# minus sign means leaving scientific_names and year out of the gathering process

quartz() # alina: not sure what this is about
# cols <- viridis_pal(option="plasma")(15) ### example on how to use viridis package
colz <- c("salmon3", "royalblue3")
leaves$code <- reorder(leaves$scientific_names, leaves$doy) ## this reorders the species by day of budburst
# hmmm not super sure^^^
leaves$colz <-ifelse(leaves$pheno=="bb_mean", "salmon3", "royalblue3")

# calculate year 2016 ----
leaves2016 <- ggplot(leaves[(leaves$year==2016),], aes(x=scientific_names, y=doy)) + geom_point(aes(color=rev(colz), shape=pheno)) +
  geom_line(col="green4", alpha=0.5) + ylab("Day of Year") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                                  axis.text.y = element_text(face = "italic"),
                                                                  axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                                  legend.box.background = element_rect(),
                                                                  panel.spacing = unit(2, "lines"),
                                                                  plot.title = element_text(color="#1B9E77"),
                                                                  legend.position = "none",
                                                                  axis.title.y = element_blank()) + labs(col="Phenophase") + 
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2016") +
  coord_flip() 

leaves2016

### Looks like something funny might be going on with the oaks... I would check out your calculations of means
# fixed the original data and solved this problem
# this could go in the accuracy discussion
# could use the flawed data to illustrate


# calculate year 2017 ----
leaves2017 <- ggplot(leaves[(leaves$year==2017),], aes(x=scientific_names, y=doy)) + geom_point(aes(color=rev(colz), shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(color="#1B9E77"),
        legend.position = "none",
        axis.title.y = element_blank()) + labs(col="Phenophase") + 
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2017") +
  coord_flip() 


# calculate year 2018 ----
leaves2018 <- ggplot(leaves[(leaves$year==2018),], aes(x=scientific_names, y=doy)) + geom_point(aes(color=rev(colz), shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(color="#1B9E77"),
        legend.position = "none",
        axis.title.y = element_blank()) + labs(col="Phenophase") + 
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2018") +
  coord_flip() 

# calculate year 2019 ----
leaves2019 <- ggplot(leaves[(leaves$year==2019),], aes(x=scientific_names, y=doy)) + geom_point(aes(color=rev(colz), shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(color="#1B9E77"),
        legend.position = "none",
        axis.title.y = element_blank()) + labs(col="Phenophase") + 
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2019") +
  coord_flip() 


# calculate year 2020 ----
leaves2020 <- ggplot(leaves[(leaves$year==2020),], aes(x=scientific_names, y=doy)) + geom_point(aes(color=rev(colz), shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(color="#1B9E77"),
        legend.position = "none",
        axis.title.y = element_blank()) + labs(col="Phenophase") + 
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2020") +
  coord_flip() 

# we probo want to set the x axis fixed 

# need to export plots across each/all year across diffo phases and understand the plot


