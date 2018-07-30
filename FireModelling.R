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


#decision tree
##http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/

mt_train = fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/golden/mt_train.csv', stringsAsFactors = FALSE)
mt_test = fread('https://raw.githubusercontent.com/lgellis/WITHackathon/master/files/golden/mt_test.csv', stringsAsFactors = FALSE)


dim(mt_train)
dim(mt_test)

names(mt_train)

## Get a baseline

#chances of success by random - 2.6% accuracy on random guess

1/length(unique(mt_train$Incident_Type))

## So The totals 
length(mt_train$Incident_Type) - 15311
table(mt_train$Incident_Type)

##What if we guessed everything as 111?  23.1 %
3541/15311*100



##############################
## Create Tree 1
##############################

names(mt_train)
attach(mt_train)

fit_tree <- rpart(Incident_Type ~  month + EVENT_TYPE + Region, 
                  data=mt_train,
                  method="class")

rpart.plot(fit_tree, type=5, extra=0, cex=0.6)

#Measure the accuracy

mt_test$predictions <- predict(fit_tree, mt_test, type = "class")
mt_test <- mt_test %>% 
  mutate(matchCol = if_else(mt_test$predictions == mt_test$Incident_Type, 1, 0))

#chances of being correct with algorithm
sum(as.numeric(mt_test$matchCol), na.rm = TRUE)/length(mt_test$IncidentID)
## 28.4%

##############################
## Create Tree 2
##############################

names(mt_train)
attach(mt_train)

fit_tree <- rpart(Incident_Type ~  month + EVENT_TYPE,
                  data=mt_train,
                  method="class")

rpart.plot(fit_tree, type=5, extra=0, cex=0.6)

#Measure the accuracy

mt_test$predictions <- predict(fit_tree, mt_test, type = "class")
mt_test <- mt_test %>% 
  mutate(matchCol = if_else(mt_test$predictions == mt_test$Incident_Type, 1, 0))

#chances of being correct with algorithm
sum(as.numeric(mt_test$matchCol), na.rm = TRUE)/length(mt_test$IncidentID)
## 29.53%


##############################
## Create Tree 3
##############################

names(mt_train)
attach(mt_train)

fit_tree <- rpart(Incident_Type ~  month + EVENT_TYPE + Region + WFO,
                  data=mt_train,
                  method="class")

rpart.plot(fit_tree, type=5, extra=0, cex=0.6)

#Measure the accuracy

mt_test$predictions <- predict(fit_tree, mt_test, type = "class")
mt_test <- mt_test %>% 
  mutate(matchCol = if_else(mt_test$predictions == mt_test$Incident_Type, 1, 0))

#chances of being correct with algorithm
sum(as.numeric(mt_test$matchCol), na.rm = TRUE)/length(mt_test$IncidentID)
## 28.46%



##############################
## Create Tree 4
##############################

names(mt_train)
attach(mt_train)

fit_tree <- rpart(Incident_Type ~  month + EVENT_TYPE + Region + WFO + CZ_NAME_STR, 
                  data=mt_train,
                  method="class")

rpart.plot(fit_tree, type=5, extra=0, cex=0.6)

#Measure the accuracy

mt_test$predictions <- predict(fit_tree, mt_test, type = "class")
mt_test <- mt_test %>% 
  mutate(matchCol = if_else(mt_test$predictions == mt_test$Incident_Type, 1, 0))

#chances of being correct with algorithm
sum(as.numeric(mt_test$matchCol), na.rm = TRUE)/length(mt_test$IncidentID)
## 28.46%




names(mt_train)
