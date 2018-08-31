install.packages("data.table")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("sqldf")
install.packages("dummies")
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("GGally")
install.packages("formattable")


library(lubridate)
library(tidyverse)
library(data.table)
library(sqldf)
library(dummies)
library(caTools)
library(rpart)
library(rpart.plot)
library(GGally)
library(formattable)



# Get the raw fire data
fCallFire= fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/KFIRS%202014-2018_Fire.csv', stringsAsFactors = FALSE)
fCallGen = fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/KFIRS%202016-2017_General.csv', stringsAsFactors = FALSE)

#Look at the data
names(fCallGen)
names(fCallFire)

#Merge the fire data
fCallFull <- merge(fCallFire, fCallGen, by="IncidentID")
fCallFull
names(fCallFull)

#Add date variables

fCallFull$mdy <-mdy(fCallFull$IN_Date)
fCallFull$year <- year(fCallFull$mdy)
fCallFull$month <- month(fCallFull$mdy)

#Filter to only 2016 & 2017 b/c that is what we have for natural disaster data

fCallFull <- fCallFull %>%
  filter(year>=2016) 

unique(fCallFull$year)

#Bring in all storm data sheets

storm1 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm1.csv', stringsAsFactors = FALSE)
storm2 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm2.csv', stringsAsFactors = FALSE)
storm3 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm3.csv', stringsAsFactors = FALSE)
storm4 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm4.csv', stringsAsFactors = FALSE)
storm5 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm5.csv', stringsAsFactors = FALSE)
storm6 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm6.csv', stringsAsFactors = FALSE)
storm7 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm7.csv', stringsAsFactors = FALSE)
storm8 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm8.csv', stringsAsFactors = FALSE)
storm9 <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/storm8.csv', stringsAsFactors = FALSE)

#Merge the storm data

stormFull <- rbind(storm1, storm2, storm3, storm4, storm5, storm6, storm7, storm8, storm9) #no need for storm 10 as it is null
dim(stormFull)
head(stormFull)
attach(stormFull)

getwd()

#Add date variables

stormFull$mdy <-mdy(stormFull$BEGIN_DATE)
stormFull$mdyPlus7 <- stormFull$mdy + days(7)
stormFull$year <- year(stormFull$mdy)
stormFull$month <- month(stormFull$mdy)
stormFull[1:1, ] # print first row to ensure correct
write.csv(stormFull, "stormFull.csv")

#Bring in the countyMap

countyMap <-fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/CountyMap.csv', stringsAsFactors = FALSE)
countyMap

#Add fire county mappings to the storm data
stormFinal <- left_join(stormFull, countyMap, by = c("CZ_NAME_STR" = "WeatherZoneCounty"))
stormFinal

attach(stormFinal)
names(stormFinal)
attach(fCallFull)
names(fCallFull)
summary(fCallFull)

#somehow had duplicate years - don't need them
stormFinal$year <- NULL
fCallFull$year <-NULL

## Full Table for analysis
fullTable <-sqldf::sqldf("SELECT a.*, b.* FROM fCallFull a
             LEFT JOIN stormFinal b 
             ON a.Call_County = b.FireCounty
             AND a.mdy >= b.mdy AND a.mdy <= b.mdyPlus7")

write.csv(fullTable, "fullTable.csv")

##

#Create the model table b/c these are all the columns we need to know

modelTable <-sqldf::sqldf("SELECT a.IncidentID, a.Region, a.IN_Date, a.Call_County, a.Incident_Type, a.Fire_General_Cause, a.Fire_heat_source, a.Primary_Ignition_Factor, a.Loss_Total, a.Burned_Buildings, a.Acres_Burned, a.Fire_Spread, a.FS_Fatality, a.Civilian_Fatality, a.FS_Injury, a.Civilian_Injury, b.* FROM fCallFull a
                          LEFT JOIN stormFinal b 
                          ON a.Call_County = b.FireCounty
                          AND a.mdy >= b.mdy AND a.mdy <= b.mdyPlus7")


##Calculate differences in totals based on having a fire call or not
#Add boolean fire event flag

modelTable <-as.data.frame(modelTable)

modelTable <- modelTable %>%
  mutate(disasterFlag = ifelse(is.na(EVENT_ID), 'No Disaster', 'Disaster'))

#summary of times where modelTable$EVENT_ID is null
summary(modelTable)
head(modelTable)

## missing some of the other totals we need.

summaryFireData <- modelTable %>% 
  group_by(disasterFlag) %>%
  summarize('Avg Total Loss' = round(mean(Loss_Total, na.rm = TRUE),2), 
            'Median Total Loss' = round(median(Loss_Total, na.rm = TRUE),2), 
            'Avg Burned Buildings' = round(mean(Burned_Buildings, na.rm = TRUE),2), 
            'Median Burned Buildings' = round(median(Burned_Buildings, na.rm = TRUE),2),
            'Avg Acres Burned' = round(mean(Acres_Burned, na.rm = TRUE),2), 
            'Median Acres Burned' = round(median(Acres_Burned, na.rm = TRUE),2),
            'Avg Fire Staff Fatality' = round(mean(FS_Fatality, na.rm = TRUE),2), 
            'Median Fire Staff Fatality' = round(median(FS_Fatality, na.rm = TRUE),2), 
            'Avg Fire Staff Injury' = round(mean(FS_Injury, na.rm = TRUE),2),
            'Median Fire Staff Injury'= round(median(FS_Injury, na.rm = TRUE),2),
            'Avg Civilian Injury' = round(mean(Civilian_Injury, na.rm = TRUE),2),
            'Median Civilian Injury' = round(median(Civilian_Injury, na.rm = TRUE),2), 
            'Avg Injuries In-Direct' = round(mean(INJURIES_INDIRECT, na.rm = TRUE),2),
            'Median Injuries In-Direct' = round(median(INJURIES_INDIRECT, na.rm = TRUE),2),
            'Avg Deaths In-Direct' = round(mean(DEATHS_INDIRECT, na.rm = TRUE),2),
            'Median Deaths In-Direct' = round(median(DEATHS_INDIRECT, na.rm = TRUE),2))

formattable(summaryFireData)
transpose <- as.data.frame(t(summaryFireData), stringsAsFactors=FALSE)


typeof(transpose)

names(transpose)<- c("Disaster", "NoDisaster")
transpose <- transpose[-1,]
formattable(transpose)


transpose$Disaster<- as.numeric(transpose$Disaster)
transpose$NoDisaster<- as.numeric(transpose$NoDisaster)

summary(transNumeric)

formattable(transpose, list(
  Disaster = formatter(
    "span",
    style = ~ style(color = ifelse(Disaster>NoDisaster, "red", "black")),
    ~ icontext(ifelse(Disaster>NoDisaster, "arrow-up", ""), Disaster))))


#add formatting with formattable

## Box plot - make multiple


ggplot(modelTable, aes(x=disasterFlag, y=Loss_Total, fill=disasterFlag)) + 
  geom_boxplot(notch=TRUE, outlier.size=1) +
  scale_fill_manual(values=c("red", "white"), name = "Disaster Indicator") +
  ggtitle("Analysis of Comparative Total Loss During Times of Disaster") +
  labs(x = NULL, y = NULL) +
  theme_bw() + theme_minimal() 

#Create the model table b/c these are all the columns we need to know

modelTable <-sqldf::sqldf("SELECT a.IncidentID, a.Region, a.IN_Date, a.Call_County, a.Incident_Type, a.Fire_General_Cause, a.Fire_heat_source, a.Primary_Ignition_Factor, b.* FROM fCallFull a
                          LEFT JOIN stormFinal b 
                          ON a.Call_County = b.FireCounty
                          AND a.mdy >= b.mdy AND a.mdy <= b.mdyPlus7")


#Only include those where 
modelTable <-modelTable %>%
  filter(!is.na(modelTable$EVENT_ID))

#Create Dummy Variables
head(modelTable)
mt <- modelTable
mt <- cbind(mt, dummy(mt$Region, sep = "_Reg_"))
mt <- cbind(mt, dummy(mt$Call_County, sep = "_Cnty_"))
mt <- cbind(mt, dummy(mt$EVENT_TYPE, sep = "_Typ_"))
names(mt) <- sub(" ", ".", names(mt))



#Split testing and training
set.seed(101)
sample = sample.split(mt, SplitRatio = .75)
mt_train = subset(mt, sample == TRUE)
mt_test  = subset(mt, sample == FALSE)

dim(mt_train)
dim(mt_test)

write.csv(fCallFull, "fireCallsAll.csv")
write.csv(stormFinal, "stormAll.csv")
write.csv(mt, "mt.csv")
write.csv(mt_train, "mt_train.csv")
write.csv(mt_test, "mt_test.csv")
write.csv(fullTable, "fullTable.csv")

#Looking at uniques and unique combos
length(unique(modelTable$Primary_Ignition_Factor))
unique(modelTable[,c('Incident_Type','Fire_heat_source')])

dim(mt)
head(mt)

