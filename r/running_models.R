rm(list=ls())

source("r/models/accRun.r")
source("r/models/featureProcessing20Hz.R")

behaviourCodeLoop()

outputData<-featureProcessing(epochs = 7)
outputData<-outputData[!outputData$Place=="land",]

table(outputData$EventIds)

load("output/1Hz/outputData_7.RData")
outputData<-outputData[complete.cases(outputData),]
table(outputData$EventIds)

accRun(outputData,Model="RF",Cores=1)

summary(table(outputData$EventIds,outputData$Place)
