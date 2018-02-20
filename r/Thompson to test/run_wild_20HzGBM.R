##### xgboost test
rm(list = ls())
#setwd("I:/Juvenile fur seal energetics")


require("caret")
require("xgboost")
require("ggplot2")
require("ROCR")
library(data.table)

Hz=20
epochs=75

#process the data for entry
##wild data
source("r/Thompson to test/featureProcessing_wild20Hz.R")
#only run if data not processed
#featureProcessing(epochs=61,endSample = 150000)

##captive data
source('r/models/captiveProcessing20Hz.R')
#featureProcessing(epochs=61,family="fur seals")


#read in the testing file
#setwd("~/Documents/r_files/Seals/wild_modles/Thompson to test 2/")
source('r/Thompson to test/accRun_test.r')
#code for GBM
source("r/Thompson to test/parallelCVxgb.r")




#list the feature data files for accRun
SealNames<-list.files("output/wild_feature_20HzCopy/")

k=2   #choose seal - look at files, don't match with usual order

##### load the wild data
#LOAD TEST DATA
load(paste0("output/wild_feature_20HzCopy/outputData_anna61.RData"))
testData<-outputData


##OR##
#LOAD THE WILD DATA
#load("outputData_barnaby75.RData")
#testData <- featureData

#LOAD CAPTIVE DATA
#load("output/captive_feature_20Hz/outputData_75.RData")
load("output/captive_feature_20Hz/outputData_fur seals61.RData")
#load("outputData_fur seals75.RData")
furseals <- outputData
load("output/captive_feature_20Hz/outputData_sea lions61.RData")
#load("outputData_sea lions75.RData")
featureData <- rbind(furseals,
                    outputData)
rm(furseals)
rm(outputData)
#remove any behaviour that occurred on land
featureData <- featureData[!featureData$Place=="land",]
#remove any grooming underwater
#featureData <- featureData[!featureData$Place=="underwater"&!featureData$EventIds=="Grooming",]

##### assign random labels to get the code to run through
EventIds <- levels(featureData$EventIds)
EventIds <- EventIds[EventIds != "Other"]
testData$EventIds <- factor(x = rep(x = 1:4, length.out = nrow(testData)), labels = EventIds)

###run the model
xgbModel <- accRun(featureData,
                   testData,
                   Model = "XGB",
                   codeTest = TRUE,
                   Parallel = FALSE,
                   K = 10,
                   SAVE = F,
                   Dummies = FALSE,
                   Cores = 1,
                   printSummary = TRUE,
                   classMax = 500)


#############################################################################
###labelling the data########################################################
#############################################################################
##### Adam's notes
# not 100% sure what you are doing here, but a lot of it is potentially redundant.
# the object xgbModel is the same as the saved data
# So all of the loading below potentially unnecessary

source("r/functions.R")
library(qdapTools)


#list and load the raw data
SealFiles <- list.files(paste0("data/wilddata"))
SealNames[k]
j=7   #make sure this matches file chosen above
load(paste("data/wilddata", SealFiles[j], sep="/"))

#subset the data to match feature data - remove once data is clean
df<-df[1:100000,]
#name the data
df$sealName <- substr(SealFiles[j],8,(nchar(SealFiles[j])-6))

###choose epoch length by which features are summarised by.
epoch <- rep(1:(nrow(df)/epochs), each=epochs)
df <- df[1:length(epoch),]
df$epoch <- epoch    

#label the events as feature data for matching
df$EventIds <- (paste0(epoch ,"_", substr(SealFiles[j],1,(nchar(SealFiles[j])-12))))

#extract test data
testData<-xgbModel$testDataSplit
testData$EventIds <- row.names(testData) 

#check it looks ok
head(testData[,135:141])

#add the final predictions to the raw data
df$finalPreds<-lookup(df$EventIds,testData[,c(139,141)])
#label the factors - assuming they are in order
df$finalPreds<-factor(df$finalPreds,labels = c("Foraging","Grooming","Resting","Travelling"))

table(df$finalPreds)
head(df)

dives<-unique(df$divenum)
diving<-NULL

for(i in 1:length(dives)){
  dat<-df[df$divenum==dives[i],]
#Calculate ODBA and VeDBA of the raw data
  dat$xma<-runmean(dat$x,3)
  dat$yma<-runmean(dat$y,3)
  dat$zma<-runmean(dat$z,3)  
  dat$dx<-dat$x-dat$xma
  dat$dy<-dat$y-dat$yma
  dat$dz<-dat$z-dat$zma 

  dat$ODBA <- (abs(dat$dx)+abs(dat$dy)+abs(dat$dz))
  dat$VeDBA <- sqrt((dat$dx)^2+(dat$dy)^2+(dat$dz)^2)
 
   diving<-rbind(diving,dat)
}

df<-diving
rm(diving)
###this is for testing only. at the moment the numbers are slightly different 
#but generally match....
Events<-unique(df$EventIds)

###ODBA and VeDBA AUC
#output<-NULL

#for(i in 1:length(Events)){
 # dat<-df[df$EventIds==Events[i],]
#  ODBA.auc   <- MESS::auc(dat$date,   dat$ODBA) 
#  VeDBA.auc   <- MESS::auc(dat$date,   dat$VeDBA)  
#  new.dat<-cbind(Events[i],VeDBA.auc,ODBA.auc)
#  output<-rbind(output,new.dat)
#}


###find the length of each of the dive bouts
x<-data.frame(unclass(rle(df$divenum)))



##########plot some examples
d=100

p1<-ggplot(data=df[df$divenum==d],aes(x=date,y=ODBA))+
  geom_line()+
  theme_bw()+
  scale_y_continuous(limits=c(0,1))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p4<-ggplot(data=df[df$divenum==d],aes(x=date,y=x))+
  geom_line(aes(y=x))+
  geom_line(aes(y=y,colour="green"))+
  geom_line(aes(y=z,colour="blue"))+
  theme_bw()+
  scale_y_continuous(limits=c(-2,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none")
p2<-ggplot(data=df[df$divenum==d],aes(x=date,y=finalPreds))+
  geom_point()+
  theme_bw()+
  ylab("")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p3<-ggplot(data=df[df$divenum==d],aes(x=date,y=-depth))+
  geom_line()+
  theme_bw()+
  #scale_y_continuous(limits=c(-2,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())

multiplot(p1,p4,p2,p3)


