# July-2, 2021
# something wonky is going on with our cleaning process


raw <- read.csv("input/status_intensity_observation_data_all_columns.csv", header = T)

basswood2019observer <- subset(raw,raw$Common_Name == "American basswood" 
                       & raw$year == 2019 & raw$Phenophase_Description == "budburst"
                       & raw$SubmittedBy_Person_ID %in% c(46197,19414,28846))

# yes observations
basswood2019observer <- subset(basswood2019observer,basswood2019observer$Phenophase_Status == 1)


# get rid of columns
basswood2019observer  <- dplyr::select(basswood2019observer , Common_Name,year,  
                                       Phenophase_Description,
                                       Phenophase_Status, Day_of_Year, Individual_ID,
                                        SubmittedBy_Person_ID, Submission_Datetime,  Observation_Date)

raw$Phenophase_Description<-ifelse(raw$Phenophase_Description=="Breaking leaf buds", "budburst", raw$Phenophase_Description)
raw$Phenophase_Description<-ifelse(raw$Phenophase_Description=="Leaves", "leafout", raw$Phenophase_Description)
raw$Phenophase_Description<-ifelse(raw$Phenophase_Description=="Flowers or flower buds", "flowers", raw$Phenophase_Description)
raw$Phenophase_Description<-ifelse(raw$Phenophase_Description=="Falling leaves", "leaf drop", raw$Phenophase_Description)
## if raw$Phenophase_Description=="Breaking leaf buds", change it to "budburst", or keep it as it is
