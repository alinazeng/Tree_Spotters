# script to plot everything (including ripe fruit doy)
# adapted from Ailene's repository at https://github.com/AileneKane/phenconstraints/blob/master/analyses
# alina.zeng(at)ubc.ca
# May-21-2021

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#load libraries ----
library(RColorBrewer)
library(dplyr)


setwd ("C:/Users/alina/Documents/git/Tree_Spotters")

# read means dataframe ----
df <- read.csv("output/treespotters_pheno_means_across_5_years_updated_May_27_allphases.csv", 
               header = T)
df.fruit <- read.csv("output/treespotters_pheno_fruit_means_across_5_years.csv", header = T)

# join data frames 
df <- full_join(df, df.fruit)

df <- rename(df, fruit_drop_mean = fruitdrop_mean, fruit_ripe_mean = ripe_mean)

# trying out different dimensions and sizes ----
png("test2.png", width=950, height=950)
# skeleton for plot
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",
     ylab=" ", ylim=c(1,52.5), yaxt='n',xlim=c(80,350),las=1)
axis(side=2,at=c(seq(from =3.5, to = 52.5, by = 3.5)),
     labels=(paste(rev(df$scientific_names))),las=1, font=3)

# to loop the process, first need to create a list of dataframes to perform the loop on. 
data_list <- split(df, seq(nrow(df))) 

# populate the plot
species <- df$scientific_name
y<-rev(seq(from =3.5, to = 52.5, by = 3.5))
for(i in 1:length(species)){
  lines(c(data_list[[i]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="seagreen",lwd=5)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="palegreen", lwd=5)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="yellow2",lwd=5)
  lines(c(data_list[[i]]$fruit_mean,data_list[[i]]$fruit_ripe_mean),c(y[i]-0.55,y[i]-0.55),col="lightgray", lwd=6)
  lines(c(data_list[[i]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-0.55,y[i]-0.55), col="orchid", lwd=5)
  lines(c(data_list[[i]]$fruit_ripe_mean,data_list[[i]]$fruit_drop_mean),c(y[i]-0.55,y[i]-0.55), col="darkorchid", lwd=5)}
legend(300,45, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1.75,lwd=2,col=c("palegreen","seagreen","yellow","orchid","gray","darkorchid"), cex=1.05)
dev.off()

# Hamamelis virginiana (watchout for its blossom time)
for (9 in 1:length(species)){
  lines(c(data_list[[9]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="seagreen",lwd=5)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="palegreen", lwd=5)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="yellow2",lwd=5)
  lines(c(data_list[[i]]$fruit_mean,data_list[[i]]$fruit_ripe_mean),c(y[i]-0.55,y[i]-0.55),col="lightgray", lwd=6)
  lines(c(data_list[[9]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-0.55,y[i]-0.55), col="orchid", lwd=5)
  lines(c(data_list[[i]]$fruit_ripe_mean,data_list[[i]]$fruit_drop_mean),c(y[i]-0.55,y[i]-0.55), col="darkorchid", lwd=5)}
