# Script to plot pheno doys and interphases across 5 years
# alina.zeng(at)ubc.ca
# codes adapted from Cat's exampleplots.R


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

df<-read.csv("output/treespotters_pheno_means_across_5_years_updated_May_18.csv",header=TRUE)


# calculate bb to leafout interphase ----
leaves_bb_leafout <- subset(df, select=c(scientific_names, bb_mean, leafout_mean))
leaves_bb_leafout <- gather(leaves_bb_leafout , pheno, doy, -scientific_names)
# minus sign means leaving scientific_names and year out of the gathering process

quartz() # alina: not sure what this is about
# cols <- viridis_pal(option="plasma")(15) ### example on how to use viridis package
colz <- c("salmon3", "royalblue3")
leaves_bb_leafout $code <- reorder(leaves_bb_leafout $scientific_names, leaves_bb_leafout $doy) ## this reorders the species by day of budburst
# hmmm not super sure^^^
leaves_bb_leafout $colz <-ifelse(leaves_bb_leafout $pheno=="bb_mean", "salmon3", "royalblue3")

# plot leaves_bb_leafout  ----
leaves_bb_leafout_plot <- ggplot(leaves_bb_leafout, aes(x=scientific_names, y=doy)) + geom_point(aes(color=rev(colz), shape=pheno)) +
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
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("Budburst to Leafout") +
  coord_flip() 

# calculate leafout to col.leaves interphase ----
leaves_leafout_col <- subset(df, select=c(scientific_names,leafout_mean,col.leaves_mean))
leaves_leafout_col <- gather(leaves_leafout_col, pheno, doy, -scientific_names)
# minus sign means leaving scientific_names and year out of the gathering process

quartz() # alina: not sure what this is about
# cols <- viridis_pal(option="plasma")(15) ### example on how to use viridis package
colz <- c("salmon3", "royalblue3")
leaves_leafout_col$code <- reorder(leaves_leafout_col$scientific_names, leaves_leafout_col$doy) ## this reorders the species by day of budburst
# hmmm not super sure^^^
leaves_leafout_col$colz <-ifelse(leaves_leafout_col$pheno=="leafout_mean", "salmon3", "royalblue3")

# plot leaves_leafout_col  ----
leaves_leafout_col_plot <- ggplot(leaves_leafout_col, aes(x=scientific_names, y=doy)) + 
  geom_point(aes(color=rev(colz), shape=pheno)) +
  geom_line(col="green4", alpha=0.5) + ylab("Day of Year") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(color="#1B9E77"),
        legend.position = "none",
        axis.title.y = element_blank()) + labs(col="Phenophase") + 
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Leafout", "Senescence")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Leafout", "Senescence")) +
  ggtitle("Leafout to Senescence") +
  coord_flip() 

# calculate col.leaves to interphase ----
leaves_leafout_col <- subset(df, select=c(scientific_names,leafout_mean,col.leaves_mean))
leaves_leafout_col <- gather(leaves_leafout_col, pheno, doy, -scientific_names)
# minus sign means leaving scientific_names and year out of the gathering process

quartz() # alina: not sure what this is about
# cols <- viridis_pal(option="plasma")(15) ### example on how to use viridis package
colz <- c("salmon3", "royalblue3")
leaves_leafout_col$code <- reorder(leaves_leafout_col$scientific_names, leaves_leafout_col$doy) ## this reorders the species by day of budburst
# hmmm not super sure^^^
leaves_leafout_col$colz <-ifelse(leaves_leafout_col$pheno=="leafout_mean", "salmon3", "royalblue3")

# plot leaves_leafout_col  ----
leaves_leafout_col_plot <- ggplot(leaves_leafout_col, aes(x=scientific_names, y=doy)) + 
  geom_point(aes(color=rev(colz), shape=pheno)) +
  geom_line(col="green4", alpha=0.5) + ylab("Day of Year") + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(face = "italic"),
        axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(color="#1B9E77"),
        legend.position = "none",
        axis.title.y = element_blank()) + labs(col="Phenophase") + 
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Leafout", "Senescence")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Leafout", "Senescence")) +
  ggtitle("Leafout to Senescence") +
  coord_flip() 



### Looks like something funny might be going on with the oaks... I would check out your calculations of means
# fixed the original data and solved this problem
# this could go in the accuracy discussion
# could use the flawed data to illustrate


