##### xgboost test
rm(list = ls())
setwd("I:/Juvenile fur seal energetics")

require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")

#source("r/captive/behaviourCodeLoop.r")
#behaviourCodeLoop(family="fur seals")

#source("r/models/captiveProcessing20Hz.R")
#featureProcessing(epochs=75,family="sea lions")


###########   101   ############

##### load the data
load("output/captive_feature_20Hz/outputData_fur seals101.RData")
furseals<-outputData
load("output/captive_feature_20Hz/outputData_sea lions101.RData")
featureData<-rbind(furseals,outputData)
featureData<-featureData[!featureData$Place=="land",]
rm(furseals)

table(featureData$Place,featureData$EventIds)
      

##### source
source("r/models/xgb code for mon/accRun.r")
source("r/models/xgb code for mon/parallelCVxgb.r")

Hz=20
epochs=101

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 300)

###########   75   ############

##### load the data
load("output/captive_feature_20Hz/outputData_fur seals75.RData")
furseals<-outputData
load("output/captive_feature_20Hz/outputData_sea lions75.RData")
featureData<-rbind(furseals,outputData)
featureData<-featureData[!featureData$Place=="land",]
rm(furseals)

table(featureData$Place,featureData$EventIds)


##### source
source("r/models/xgb code for mon/accRun.r")
source("r/models/xgb code for mon/parallelCVxgb.r")

Hz=20
epochs=75

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 300)

###########   7   ############

##### load the data
load("output/captive_feature_20Hz/outputData_fur seals7.RData")
furseals<-outputData
load("output/captive_feature_20Hz/outputData_sea lions7.RData")
featureData<-rbind(furseals,outputData)
featureData<-featureData[!featureData$Place=="land",]
rm(furseals)

table(featureData$Place,featureData$EventIds)


##### source
source("r/models/xgb code for mon/accRun.r")
source("r/models/xgb code for mon/parallelCVxgb.r")

Hz=20
epochs=7

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 300)

  