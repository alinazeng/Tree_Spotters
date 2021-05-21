# script to plot 
# adapted from Ailene's repository at https://github.com/AileneKane/phenconstraints/blob/master/analyses
# alina.zeng(at)ubc.ca


#Analyses and figures for Sally's Phenology data
#data collection occured during the 2015 growing season at Arnold ARboretum, for Sally Gee's thesis data
#by Sally (Started 2016) and added to by Ailene (beginning in Feb 2017)
#June 2017
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
# Set working directory: 
if(length(grep("ailene", getwd()))>0) {setwd("~/git/phenconstraints")}
setwd("C:/Users/alina/Documents/git/Tree_Spotters")


#load libraries ----
library(RColorBrewer)
library(dplyr)

#Read in data:
#dat<-read.csv("data/growingseason_doy.csv", header=T)
#dat2 has additional phenophases in it; otherwise i think the two datasets are the same?
dat2<-read.csv("analyses/output/growingseason_doy2.csv", header = T)

#calculate start of each phase and interphase durations:
source("analyses/source/phase_start_and_inter_species.R")

#Make "Phenograms": Plots when different phenophases happened

#Each species gets a different color, using ramping
#cols <- rev(colorRampPalette(brewer.pal(9,"YlOrRd"))(25))#red to yellow with later flowering dates
cols <- colorRampPalette(brewer.pal(9,"YlOrRd"))(25)#yellow to red with later flowering dates

quartz(height=8,width=10)
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",ylab=" ", ylim=c(1,50), yaxt='n',xlim=c(110,385),las=1)
axis(side=2,at=c(seq(from =2, to = 50, by = 2)),labels=(paste(rev(names(fLDstartm)))),las=1, font=3)

# check this out ---
df <- read.csv("output/treespotters_pheno_means_across_5_years_updated_May_18.csv", header = T)

df$scientific_names

X11(height=8,width=10)
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",
     ylab=" ", ylim=c(1,52.5), yaxt='n',xlim=c(80,385),las=1)
  axis(side=2,at=c(seq(from =3.5, to = 52.5, by = 3.5)),
      labels=(paste(rev(df$scientific_names))),las=1, font=3)

  # rest

#Start with first to flower
species<-names(fFLstartm)
y<-rev(seq(from =2, to = 50, by = 2))
for(i in 1:length(species)){
  lines(c(fLOstartm[i],fSENendm[i]),c(y[i],y[i]), col="seagreen",lwd=3)
  lines(c(fFLstartm[i],fFRendm[i]),c(y[i]-0.4,y[i]-0.4),col="lightgray", lwd=4)
  lines(c(fFLstartm[i],fFRstartm[i]),c(y[i]-0.4,y[i]-0.4), col="orchid", lwd=3)
  lines(c(fLDstartm[i],fLDendm[i]),c(y[i],y[i]), col="palegreen", lwd=3)
  lines(c(fRFRstartm[i],fRFRendm[i]),c(y[i]-0.4,y[i]-0.4), col="darkorchid", lwd=3)
  lines(c(fSENstartm[i],fSENendm[i]),c(y[i],y[i]), col="yellow2",lwd=3)
}
legend(325,50, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1,lwd=2,col=c("palegreen","seagreen","yellow","orchid","gray","darkorchid"), cex=.85)