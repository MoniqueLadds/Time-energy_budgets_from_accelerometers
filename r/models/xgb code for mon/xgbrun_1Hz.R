
source(file = "r/models/featureProcessing1Hz.R")

##### load the data

#furseal<-featureProcessing(epochs = 20, family="fur seals")
#sealion<-featureProcessing(epochs = 20,family="sea lions")


#########################################
##### xgboost test 30 epochs##############
#########################################
rm(list = ls())


require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")


load(file = "output/1Hz/outputData_sea lions30.RData")      
furseal<-outputData
table(furseal$EventIds,furseal$Place)
load(file = "output/1Hz/outputData_fur seals30.RData")
sealion<-outputData
table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)


##### source
source("r/models/xgb code for mon/accRun1Hz.R")
source("r/models/xgb code for mon/parallelCVxgb.r")

epochs = 30
Hz = 1

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMin = 100)


#########################################
##### xgboost test 20 epochs##############
#########################################

rm(list = ls())


require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")


load(file = "output/1Hz/outputData_sea lions20.RData")      
furseal<-outputData
table(furseal$EventIds,furseal$Place)
load(file = "output/1Hz/outputData_fur seals20.RData")
sealion<-outputData
table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)


##### source
source("r/models/xgb code for mon/accRun1Hz.R")
source("r/models/xgb code for mon/parallelCVxgb.r")

epochs=20
Hz=1

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMin = 200)

#########################################
##### xgboost test 15 epochs##############
#########################################


rm(list = ls())

setwd("I:/Juvenile fur seal energetics")
require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")


load(file = "output/1Hz/outputData_fur seals15.RData")
furseal<-outputData
table(furseal$EventIds,furseal$Place)
sealion<-outputData
load(file = "output/1Hz/outputData_sea lions15.RData")
table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)


##### source
source("r/models/xgb code for mon/accRun.R")
source("r/models/xgb code for mon/parallelCVxgb.r")

epochs=15
Hz=1

xgbModel <- accRun(featureData,
                   Model = "XGB",
                   codeTest = FALSE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = TRUE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 100)

#########################################
##### xgboost test 10 epochs##############
#########################################

rm(list = ls())


require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")


load(file = "output/1Hz/outputData_sea lions10.RData")      
furseal<-outputData
table(furseal$EventIds,furseal$Place)
load(file = "output/1Hz/outputData_fur seals10.RData")
sealion<-outputData
table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)


##### source
source("r/models/xgb code for mon/accRun.R")
source("r/models/xgb code for mon/parallelCVxgb.r")

epochs=10
Hz=1
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

#########################################
##### xgboost test 6 epochs##############
#########################################

rm(list = ls())

setwd("I:/Juvenile fur seal energetics")
require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")


load(file = "output/1Hz/outputData_sea lions6.RData")      
furseal<-outputData
table(furseal$EventIds,furseal$Place)
load(file = "output/1Hz/outputData_fur seals6.RData")
sealion<-outputData
table(sealion$EventIds,sealion$Place)
featureData<-rbind(furseal,sealion)


##### source
source("r/models/xgb code for mon/accRun.R")
source("r/models/xgb code for mon/parallelCVxgb.r")

epochs=6
Hz=1

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