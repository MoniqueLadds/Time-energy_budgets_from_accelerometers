##### xgboost test
rm(list = ls())
#setwd("I:/Juvenile fur seal energetics")


require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")
library(data.table)

Hz=1
epochs=21

#process the data for entry
##wild data
source("r/1Hz_models/featureProcessing_wild1Hz.R")
#only run if data not processed
featureProcessing(epochs=21)

##captive data
source('r/1Hz_models/featureProcessing1Hz.R')
#featureProcessing(epochs=15,family="fur seals")


#list the feature data files for accRun
SealNames<-list.files("output/wild_feature_1Hz/")
SealNames<-SealNames[!SealNames=="15epochs"]
SealNames<-SealNames[!SealNames=="samples"]

k=1   #choose seal - look at files, don't match with usual order

##### load the wild data
#LOAD TEST DATA
load(paste0("output/wild_feature_1Hz/",SealNames[k]))
testData<-featureData


#LOAD CAPTIVE DATA
load("output/captive_feature_1Hz/outputData_fur seals15.RData")
furseals <- outputData
load("output/captive_feature_1Hz/outputData_sea lions15.RData")
featureData <- rbind(furseals,
                    outputData)
featureData$EventIds<-factor(featureData$EventIds,labels=c("Foraging","Grooming","Other","Resting","Travelling"))
featureData<-subset(featureData,!c(EventIds=="Foraging"&Place=="land"))
rm(furseals)
rm(outputData)

#remove foraging
#levels(featureData$EventIds)[levels(featureData$EventIds)=="Foraging"] <- "Travelling"

##### assign random labels to get the code to run through
EventIds <- levels(featureData$EventIds)
EventIds <- EventIds[EventIds != "Other"]
testData$EventIds <- factor(x = rep(x = 1:4, length.out = nrow(testData)), labels = EventIds)

#read in the testing file
#source('r/1Hz_models/accRun1Hz.R')
source('r/1Hz_models/accRun_test.R')
#code for GBM
source("r/1Hz_models/parallelCVxgb.r")

###run the model
xgbModel <- accRun(featureData,
                   testData,
                   Model = "XGB",
                   codeTest = TRUE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 50)



