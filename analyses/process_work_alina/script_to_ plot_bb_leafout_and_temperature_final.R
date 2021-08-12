
ggsave(file="doy_mean_all_year.svg", width=10, height=10)# script to plot bb leafout and temperature final
# July 27
# alina zeng

# Housekeeping ----
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Libraries needed for formatting and tidying data ----
library(dplyr)
library(tidyr)
library(lubridate)
library(dygraphs)
library(Cairo)
library(webshot)
library(dplyr)
library(tidyr)
library(ggplot2)


# first need to import and take average of data
doy <- read.csv("output/cleaningJuly07.csv", header = T)

# take average across EVERYTHING
doy_mean_all <- doy %>% group_by(year, phase) %>%  summarise(doy_mean_all = mean(doy_mean), 
                                                             range_mean=mean(interquartile_range), min_mean = mean(first_doy), 
                                                             max_mean = mean(last_doy))

ggplot(doy_mean_all[(doy_mean_all$phase %in% c("budburst","leafout","Colored leaves","flowers","Fruits")),], 
       aes(x = year, y = doy_mean_all, color = phase, fill = phase)) +  
  geom_line(alpha=0.5)+
  geom_linerange(aes(ymin = min_mean, ymax = max_mean))+
  scale_y_continuous(breaks=seq(0,330,15))+
  scale_fill_manual(values = c("#BBDAC4", "#EFAC77","#F1BFC8","#D7C9A6","#6C8C66"),
                    labels = c("Budburst", "Colored leaves","Flowers","Fruits","Leafout")) +
  theme_classic()+
  coord_flip() +
  labs(x = "\n Year", y = "Day of year \n")  
ggsave(file="doy_mean_all_year_flipped.svg", width=10, height=10)


# try out dots (or lines)
png(filename="xxx.png", 
    type="cairo", ### this helps with resolution, love it!!
    units="in", 
    width=8, 
    height=6,     # color = phase
    res=300)
ggplot(doy_mean_all[(doy_mean_all$phase %in% c("budburst","leafout","Colored leaves","flowers","Fruits")),], 
       aes(x = year, y = doy_mean_all, color = phase, fill = phase)) +  
  geom_line(alpha=0.5)+
  scale_y_continuous(breaks=seq(0,330,15))+
  scale_fill_manual(values = c("#BBDAC4", "#EFAC77","#F1BFC8","#D7C9A6","#6C8C66"),
                    labels = c("Budburst", "Colored leaves","Flowers","Fruits","Leafout")) +
  theme_classic()+
  labs(x = "\n Year", y = "Day of year \n")     # \n adds space before x and after y axis text
# dev.off()
ggsave(file="doy_mean_all_year.svg", width=10, height=10)


# plots temperature

# refer to script to plot climate data


# plot doys by species by year

# take average of species by year
doy_mean_spp <- doy %>% group_by(year, phase,common_name ) %>%  summarise(doy_mean_spp = mean(doy_mean))
doy_mean_spp <- doy_mean_spp %>% tidyr::spread(phase, doy_mean_spp)

# rename column
doy_mean_spp <- doy_mean_spp %>% rename(bb_mean = budburst,flower_mean = flowers,
                            fruit_mean = Fruits, leafout_mean =  leafout,
                            col.leaves_mean = "Colored leaves", leafdrop_mean = "leaf drop",
                            flower_open_mean = "Open flowers", flower_pollen_mean = "Pollen release (flowers)",
                            fruit_ripe_mean = "Ripe fruits",  fruit_drop_mean =  "Recent fruit or seed drop")

df <- read.csv("output/treespotters_pheno_means_across_5_years_updated_May_27_allphases.csv", 
               header = T)


# join scientific names
names <- read.csv("input/names.csv", header = T)
names <- rename(names, common_name = Common_Name,scientific_names = Scientific_Names)
doy_mean_spp <- full_join(doy_mean_spp, names)

# subset and make df for each year
doy_mean_spp_2016 <- doy_mean_spp[doy_mean_spp$year == 2016,]
doy_mean_spp_2017 <- doy_mean_spp[doy_mean_spp$year == 2017,]
# get rid of Viburnum
doy_mean_spp_2017 <- doy_mean_spp_2017[doy_mean_spp_2017$scientific_names!= "Viburnum nudum",]
doy_mean_spp_2018 <- doy_mean_spp[doy_mean_spp$year == 2018,]
doy_mean_spp_2019 <- doy_mean_spp[doy_mean_spp$year == 2019,]
doy_mean_spp_2020 <- doy_mean_spp[doy_mean_spp$year == 2020,]

doy_mean_spp_2016 <- doy_mean_spp_2016 %>% arrange(scientific_names)
doy_mean_spp_2017 <- doy_mean_spp_2017 %>% arrange(scientific_names)
doy_mean_spp_2018 <- doy_mean_spp_2018 %>% arrange(scientific_names)
doy_mean_spp_2019 <- doy_mean_spp_2019 %>% arrange(scientific_names)
doy_mean_spp_2020 <- doy_mean_spp_2020 %>% arrange(scientific_names)

png(filename="doy_mean_all_species_2016.png", 
    type="cairo",    ### this helps with resolution A LOT (windows sux)
    units="in", 
    width=14, 
    height=12, 
    res=300)
# skeleton for plot
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",
     ylab=" ", ylim=c(1,38.5), yaxt='n',xlim=c(80,330),las=1)
axis(side=2,at=c(seq(from =3.5, to = 38.5, by = 3.5)),
     labels=(paste(rev(doy_mean_spp_2016$scientific_names))),las=1, font=3)



# to loop the process, first need to create a list of dataframes to perform the loop on. 
data_list <- split(doy_mean_spp_2016, seq(nrow(doy_mean_spp_2016))) 

# populate the plot
species <- doy_mean_spp_2016$scientific_names
y<-rev(seq(from =3.5, to = 38.5, by = 3.5))
for(i in 1:length(species)){
  lines(c(data_list[[i]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="#6C8C66",lwd=5)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="#BBDAC4", lwd=5)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="#EFAC77",lwd=5)
  lines(c(data_list[[i]]$fruit_mean,data_list[[i]]$fruit_ripe_mean),c(y[i]-1.05,y[i]-1.00),col="#D7C9A6", lwd=6.5)
  lines(c(data_list[[i]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-1.05,y[i]-1.05), col="#F1BFC8", lwd=5)
  lines(c(data_list[[i]]$fruit_ripe_mean,data_list[[i]]$fruit_drop_mean),c(y[i]-1.05,y[i]-1.05), col="#64433f", lwd=5)}
# legend(300,45, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1.75,lwd=2,col=c("palegreen","seagreen","yellow","orchid","gray","darkorchid"), cex=1.05)
dev.off()



# 2017

png(filename="doy_mean_all_species_2017.png", 
    type="cairo",    ### this helps with resolution A LOT (windows sux)
    units="in", 
    width=14, 
    height=12, 
    res=300)
# skeleton for plot
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",
     ylab=" ", ylim=c(1,38.5), yaxt='n',xlim=c(80,330),las=1)
axis(side=2,at=c(seq(from =3.5, to = 38.5, by = 3.5)),
     labels=(paste(rev(doy_mean_spp_2017$scientific_names))),las=1, font=3)



# to loop the process, first need to create a list of dataframes to perform the loop on. 
data_list <- split(doy_mean_spp_2017, seq(nrow(doy_mean_spp_2017))) 

# populate the plot
species <- doy_mean_spp_2017$scientific_names
y<-rev(seq(from =3.5, to = 38.5, by = 3.5))
for(i in 1:length(species)){
  lines(c(data_list[[i]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="#6C8C66",lwd=5)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="#BBDAC4", lwd=5)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="#EFAC77",lwd=5)
  lines(c(data_list[[i]]$fruit_mean,data_list[[i]]$fruit_ripe_mean),c(y[i]-1.05,y[i]-1.00),col="#D7C9A6", lwd=6.5)
  lines(c(data_list[[i]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-1.05,y[i]-1.05), col="#F1BFC8", lwd=5)
  lines(c(data_list[[i]]$fruit_ripe_mean,data_list[[i]]$fruit_drop_mean),c(y[i]-1.05,y[i]-1.05), col="#64433f", lwd=5)}
# legend(300,45, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1.75,lwd=2,col=c("palegreen","seagreen","yellow","orchid","gray","darkorchid"), cex=1.05)
dev.off()


# skipped 2018 cuz not enoug data
# 2019

png(filename="doy_mean_all_species_2019.png", 
    type="cairo",    ### this helps with resolution A LOT (windows sux)
    units="in", 
    width=14, 
    height=12, 
    res=300)
# skeleton for plot
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",
     ylab=" ", ylim=c(1,52.5), yaxt='n',xlim=c(80,330),las=1)
axis(side=2,at=c(seq(from =3.5, to = 52.5, by = 3.5)),
     labels=(paste(rev(doy_mean_spp_2019$scientific_names))),las=1, font=3)



# to loop the process, first need to create a list of dataframes to perform the loop on. 
data_list <- split(doy_mean_spp_2019, seq(nrow(doy_mean_spp_2019))) 

# populate the plot
species <- doy_mean_spp_2019$scientific_names
y<-rev(seq(from =3.5, to = 52.5, by = 3.5))
for(i in 1:length(species)){
  lines(c(data_list[[i]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="#6C8C66",lwd=5)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="#BBDAC4", lwd=5)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="#EFAC77",lwd=5)
  lines(c(data_list[[i]]$fruit_mean,data_list[[i]]$fruit_ripe_mean),c(y[i]-1.05,y[i]-1.00),col="#D7C9A6", lwd=6.5)
  lines(c(data_list[[i]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-1.05,y[i]-1.05), col="#F1BFC8", lwd=5)
  lines(c(data_list[[i]]$fruit_ripe_mean,data_list[[i]]$fruit_drop_mean),c(y[i]-1.05,y[i]-1.05), col="#64433f", lwd=5)}
# legend(300,45, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1.75,lwd=2,col=c("palegreen","seagreen","yellow","orchid","gray","darkorchid"), cex=1.05)
dev.off()


# 2020

png(filename="doy_mean_all_species_2020.png", 
    type="cairo",    ### this helps with resolution A LOT (windows sux)
    units="in", 
    width=14, 
    height=12, 
    res=300)
# skeleton for plot
par(mai=c(1,3,.1,.1), omi=c(1.2,.1,.1,.2))
plot(8,10, type="p", cex=.8,pch=21, col="white", bty="L", xlab="Day of Year",
     ylab=" ", ylim=c(1,52.5), yaxt='n',xlim=c(80,330),las=1)
axis(side=2,at=c(seq(from =3.5, to = 52.5, by = 3.5)),
     labels=(paste(rev(doy_mean_spp_2020$scientific_names))),las=1, font=3)



# to loop the process, first need to create a list of dataframes to perform the loop on. 
data_list <- split(doy_mean_spp_2020, seq(nrow(doy_mean_spp_2020))) 

# populate the plot
species <- doy_mean_spp_2020$scientific_names
y<-rev(seq(from =3.5, to = 52.5, by = 3.5))
for(i in 1:length(species)){
  lines(c(data_list[[i]]$leafout_mean,data_list[[i]]$col.leaves_mean),c(y[i],y[i]), col="#6C8C66",lwd=5)
  lines(c(data_list[[i]]$bb_mean,data_list[[i]]$leafout_mean),c(y[i],y[i]), col="#BBDAC4", lwd=5)
  lines(c(data_list[[i]]$col.leaves_mean,data_list[[i]]$leafdrop_mean),c(y[i],y[i]), col="#EFAC77",lwd=5)
  lines(c(data_list[[i]]$fruit_mean,data_list[[i]]$fruit_ripe_mean),c(y[i]-1.05,y[i]-1.00),col="#D7C9A6", lwd=6.5)
  lines(c(data_list[[i]]$flower_mean,data_list[[i]]$fruit_mean),c(y[i]-1.05,y[i]-1.05), col="#F1BFC8", lwd=5)
  lines(c(data_list[[i]]$fruit_ripe_mean,data_list[[i]]$fruit_drop_mean),c(y[i]-1.05,y[i]-1.05), col="#64433f", lwd=5)}
# legend(300,45, legend=c("budburst","leafout", "senescence","in flower","fruit developing", "ripe fruit"), lty=1.75,lwd=2,col=c("palegreen","seagreen","yellow","orchid","gray","darkorchid"), cex=1.05)
dev.off()




