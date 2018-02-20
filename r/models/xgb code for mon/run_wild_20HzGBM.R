##### xgboost test
rm(list = ls())
#setwd("~/Documents/r_files/Seals/SealsCode1stitteration/")

require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")
library(data.table)

#process the data for entry
source("r/models/featureProcessing_wild20Hz.R")

#read in the testing file
source('r/models/accRun_test.r')
#code for GBM
source("r/models/parallelCVxgb.r")

#only run if data not processed
featureProcessing(epochs=21)

##### load the data
SealNames<-list.files("output/wild_feature_20Hz/")
j=1
#LOAD TEST DATA
load(paste0("output/wild_feature_20Hz/",SealNames[j]))
testData<-outputData
#testData$EventIds<-as.factor(row.names(testData))
#testData<-do.call(rbind.data.frame, featureData)
#testData$EventIds<-(seq(1,nrow(testData),1))

#LOAD CAPTIVE DATA
load("output/captive_feature_20Hz/outputData_fur seals75.RData")
furseals<-outputData
load("output/captive_feature_20Hz/outputData_sea lions75.RData")
featureData<-rbind(furseals,outputData)
rm(furseals)
rm(outputData)
featureData<-featureData[!featureData$Place=="land",]

##### assign random labels to get the code to run through
EventIds <- levels(featureData$EventIds)
EventIds <- EventIds[EventIds != "Other"]
testData$EventIds <- factor(x = rep(x = 1:4, length.out = nrow(testData)), labels = EventIds)

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
                   classMax = 1000)