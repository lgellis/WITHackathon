install.packages("data.table")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("sqldf")
install.packages("dummies")
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")


library(lubridate)
library(tidyverse)
library(data.table)
library(sqldf)
library(dummies)
library(caTools)
library(rpart)
library(rpart.plot)

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






