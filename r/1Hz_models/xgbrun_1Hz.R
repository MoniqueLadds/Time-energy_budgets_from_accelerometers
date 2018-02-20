


##### load the data

##captive data
source('r/1Hz_models/featureProcessing1Hz.R')
featureProcessing(epochs=21,family="fur seals")


#########################################
##### xgboost test 7 epochs##############
#########################################
rm(list = ls())
#setwd("I:/Juvenile fur seal energetics")


require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")


#LOAD CAPTIVE DATA
load(file = "output/captive_feature_1Hz/outputData_sea lions7.RData")      
furseal<-outputData
table(furseal$EventIds,furseal$Place)
load(file = "output/captive_feature_1Hz/outputData_fur seals7.RData")
sealion<-outputData
table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)
featureData<-subset(featureData,!c(EventIds=="Foraging"&Place=="land"))


#featureData$EventIds<-row.names(featureData)

##### source
source("r/1Hz_models/accRun.R")
source("r/1Hz_models/parallelCVxgb.r")

epochs = 7
Hz = 1

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 100)



#########################################
##### xgboost test 15 epochs##############
#########################################


rm(list = ls())

setwd("I:/Juvenile fur seal energetics")
require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")


load(file = "output/captive_feature_1Hz/outputData_sea lions15.RData")      
furseal<-outputData
table(furseal$EventIds,furseal$Place)
load(file = "output/captive_feature_1Hz/outputData_fur seals15.RData")
sealion<-outputData
table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)
featureData<-subset(featureData,!c(EventIds=="Foraging"&Place=="land"))


##### source
source("r/1Hz_models/accRun.R")
source("r/1Hz_models/parallelCVxgb.r")


epochs=15
Hz=1

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = TRUE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 50)

#########################################
##### xgboost test 21 epochs##############
#########################################

rm(list = ls())


require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")


load(file = "output/captive_feature_1Hz/outputData_sea lions21.RData")      
furseal<-outputData
#table(furseal$EventIds,furseal$Place)
load(file = "output/captive_feature_1Hz/outputData_fur seals21.RData")
sealion<-outputData
#table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)
featureData<-subset(featureData,!c(EventIds=="Foraging"&Place=="land"))


##### source
source("r/1Hz_models/accRun.R")
source("r/1Hz_models/parallelCVxgb.r")


epochs=21
Hz=1
xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 50)
