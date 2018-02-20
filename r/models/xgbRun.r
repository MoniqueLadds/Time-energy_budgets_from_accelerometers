
                  #########################################
                  ##### xgboost test 7 epochs##############
                  #########################################
rm(list = ls())


require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")

source(file = "r/models/featureProcessing20Hz.R")

##### load the data

#furseal<-featureProcessing(epochs = 7, family="fur seals")
#sealion<-featureProcessing(epochs = 7,family="sea lions")

load(file = "output/20Hz/outputData_fur seals7.RData")      
furseal<-outputData
table(furseal$EventIds,furseal$Place)
load(file = "output/20Hz/outputData_sea lions7.RData")
sealion<-outputData
table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)
featureData<-featureData[!featureData$Place=="land",]


##### source
source("r/models/xgb code for mon/accRun.r")
source("r/models/xgb code for mon/parallelCVxgb.r")

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 6500)


                  #########################################
                  ##### xgboost test 13 epochs##############
                  #########################################
                  

rm(list=ls())
require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")

source(file = "r/models/featureProcessing20Hz.R")

#furseal<-featureProcessing(epochs = 13, family="fur seals")
#sealion<-featureProcessing(epochs = 13,family="sea lions")

load(file = "output/20Hz/outputData_fur seals13.RData")
furseal<-outputData
load(file = "output/20Hz/outputData_sea lions13.RData")
sealion<-outputData
featureData<-rbind(furseal,sealion)
featureData<-featureData[!featureData$Place=="land",]

##### source
source("r/models/xgb code for mon/accRun.r")
source("r/models/xgb code for mon/parallelCVxgb.r")

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 3000)
                  
                  

                  #########################################
                  ##### xgboost test 25 epochs##############
                  #########################################

rm(list=ls())
require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")

source(file = "r/models/featureProcessing20Hz.R")

#furseal<-featureProcessing(epochs = 25, family="fur seals")
#sealion<-featureProcessing(epochs = 25,family="sea lions")
                  
                  load(file = "output/20Hz/outputData_fur seals25.RData")
                  furseal<-outputData
                  load(file = "output/20Hz/outputData_sea lions25.RData")
                  sealion<-outputData
                  featureData<-rbind(furseal,sealion)
                  featureData<-featureData[!featureData$Place=="land",]

##### source
source("r/models/xgb code for mon/accRun.r")
source("r/models/xgb code for mon/parallelCVxgb.r")

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 1000)


                  
                  #########################################
                  ##### xgboost test 75 epochs##############
                  #########################################
                  
                  
rm(list=ls())
require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")

#furseal<-featureProcessing(epochs = 75, family="fur seals")
#sealion<-featureProcessing(epochs = 75,family="sea lions")
                  load(file = "output/20Hz/outputData_fur seals75.RData")
                  furseal<-outputData
                  load(file = "output/20Hz/outputData_sea lions75.RData")
                  sealion<-outputData
                  featureData<-rbind(furseal,sealion)
                  featureData<-featureData[!featureData$Place=="land",]

##### source
source("r/models/xgb code for mon/accRun.r")
source("r/models/xgb code for mon/parallelCVxgb.r")

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 500)

  