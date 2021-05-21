# interesting...
# migrating everything from one project to the other
# May-21-2021

# script to plot 
# adapted from Ailene's repository at https://github.com/AileneKane/phenconstraints/blob/master/analyses
# alina.zeng(at)ubc.ca

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#load libraries ----
library(RColorBrewer)
library(dplyr)


# read means dataframe
df <- read.csv("output/treespotters_pheno_means_across_5_years_updated_May_18.csv", 
               header = T)

# trying out different dimensions and sizes ----
png("test1.png", width=950, height=950)
# skeleton for plot
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",
     ylab=" ", ylim=c(1,52.5), yaxt='n',xlim=c(80,385),las=1)
axis(side=2,at=c(seq(from =3.5, to = 52.5, by = 3.5)),
     labels=(paste(rev(df$scientific_names))),las=1, font=3)

# to loop the process, first need to create a list of dataframes to perform the loop on. 

data_list <- split(df, seq(nrow(df))) 

# populate the plot

species <- df$scientific_name
y<-rev(seq(from =3.5, to = 52.5, by = 3.5))
for(i in 1:length(species)){
  lines(c(data_list[[i]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(data_list[[i]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-0.7,y[i]-0.7), col="orchid", lwd=4)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="palegreen", lwd=4)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="yellow2",lwd=4)}
dev.off()

# size option 2 ----

png("test2.png", width=950, height=950)
# skeleton for plot
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",
     ylab=" ", ylim=c(1,45), yaxt='n',xlim=c(80,385),las=1)
axis(side=2,at=c(seq(from = 3, to = 45, by = 3)),
     labels=(paste(rev(df$scientific_names))),las=1, font=3)

# to loop the process, first need to create a list of dataframes to perform the loop on. 

data_list <- split(df, seq(nrow(df))) 

# populate the plot

species <- df$scientific_name
y<-rev(seq(from =3, to = 45, by = 3))
for(i in 1:length(species)){
  lines(c(data_list[[i]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(data_list[[i]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-0.5,y[i]-0.5), col="orchid", lwd=4)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="palegreen", lwd=4)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="yellow2",lwd=4)}
dev.off()


# trying out different dimensions and sizes ----
png("test3.png", width=950, height=950)
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
  lines(c(data_list[[i]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="seagreen",lwd=4)
  lines(c(data_list[[i]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-0.5,y[i]-0.5), col="orchid", lwd=4)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="palegreen", lwd=4)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="yellow2",lwd=4)}
dev.off()
