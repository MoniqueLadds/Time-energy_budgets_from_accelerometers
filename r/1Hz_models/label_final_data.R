rm(list=ls())


setwd("I:/Juvenile fur seal energetics")



#############################################################################
###labelling the data########################################################
#############################################################################
source("r/functions.R")
source('r/1Hz_models/split_long_dives_1Hz.R')

library(qdapTools)
library(MESS)
library(caTools)
library(gridExtra)
library(grid)
library(ggplot2)
library(dplyr)
library(lubridate)

outputs<-list.files("output/outputDirectory/wild/1Hz-nofeature/")

for (i in 1:length(outputs)){
load(paste0("output/outputDirectory/wild/1Hz-nofeature/",outputs[i]))
  
  cat(paste0("output/outputDirectory/wild/",outputs[i]))
seal<-substr(outputs[i],1,(gregexpr(" ",outputs[i])[[1]][1])-1)

##READ IN START/END TIMES
deets<-read.csv("start.end_times.csv",stringsAsFactors=FALSE)
deets$start.date<-as.POSIXct(deets$start.date,format="%d/%m/%Y %H:%M")
deets$end.date<-as.POSIXct(deets$end.date,format="%d/%m/%Y %H:%M")
deets$start.secs<-as.numeric(seconds(deets$start.date))
deets$end.secs<-as.numeric(seconds(deets$end.date))

j<-paste0(lookup(seal,deets[,c("seal","id")]),".RData")


#list and load the raw data
SealFiles <- list.files(paste0("data/wilddata_1Hz/"))
load(paste("data/wilddata_1Hz/", j, sep="/"))

##clean and label the data
acc<-split_data(acc,j)

#name the data
acc$sealName <- seal

##Merge the deets
acc<-merge(acc,deets[,c(1,8:13)],all.x=T, by.x="sealName",by.y="seal")

Hz=1
epochs=21

###choose epoch length by which features are summarised by.
epoch <- rep(1:(nrow(acc)/epochs), each=epochs)
acc <- acc[1:length(epoch),]
acc$epoch <- epoch    

#label the events as feature acca for matching
acc$EventIds <- factor(paste0(substr(j,1,nchar(j)-12),"_", epoch ))

#extract test acca
testData<-outputData$testDataSplit
testData$EventIds <- row.names(testData) 

#check it looks ok
#head(testData[,40:50])

#add the final predictions to the raw acca
acc$finalPreds<-lookup(acc$EventIds,testData[,c("EventIds","predsTest")])
#label the factors - assuming they are in order
acc$finalPreds<-factor(acc$finalPreds,labels = c("Foraging","Grooming","Resting","Travelling"))

cat(table(acc$finalPreds),"\n")


save(acc,file=paste0("data/labelled_wilddata_nofeature/",j))

}



######################################################################
##summarise machine learning stats##

library(dplyr)

options(digits=5,scipen = 100)

outputs<-list.files("output/outputDirectory/wild/1Hz-nofeature/")
Travels<-NULL
Rests<-NULL
Grooms<-NULL
Forages<-NULL
totals<-NULL

for(l in 1:length(outputs)){
  
  load(paste0("output/outputDirectory/wild/1Hz-nofeature/",outputs[l]))
  
  testData<-outputData$testDataSplit

EventProbs<-testData$prob
EventProbs$Event <- colnames(EventProbs)[max.col(EventProbs, ties.method = 'first')]
EventProbs$EventValue <- apply(EventProbs[,1:4],1,max)

Travel<-EventProbs[EventProbs$EventValue<0.8&EventProbs$Event=="Travelling",]
Travel$DiffEvent <- colnames(Travel)[max.col(Travel[,1:3], ties.method = 'first')]
Travel$DiffEventValue <-   apply(Travel[,1:3],1,max)
Travel$Diff<-Travel$EventValue-Travel$DiffEventValue

Travels<-rbind(Travels,Travel)

Forage<-EventProbs[EventProbs$EventValue<0.8&EventProbs$Event=="Foraging",]
Forage$DiffEvent <- colnames(Forage[,2:4])[max.col(Forage[,2:4], ties.method = 'first')]
Forage$DiffEventValue <-   apply(Forage[,2:4],1,max)
Forage$Diff<-Forage$EventValue-Forage$DiffEventValue

Forages<-rbind(Forages,Forage)

Groom<-EventProbs[EventProbs$EventValue<0.8&EventProbs$Event=="Grooming",]
Groom$DiffEvent <- colnames(Groom[,c(1,3:4)])[max.col(Groom[,c(1,3:4)], ties.method = 'first')]
Groom$DiffEventValue <-   apply(Groom[,c(1,3:4)],1,max)
Groom$Diff<-Groom$EventValue-Groom$DiffEventValue

Grooms<-rbind(Grooms,Groom)

Rest<-EventProbs[EventProbs$EventValue<0.8&EventProbs$Event=="Resting",]
Rest$DiffEvent <- colnames(Rest[,c(1:2,4)])[max.col(Rest[,c(1:2,4)], ties.method = 'first')]
Rest$DiffEventValue <-   apply(Rest[,c(1:2,4)],1,max)
Rest$Diff<-Rest$EventValue-Rest$DiffEventValue

Rests<-rbind(Rests, Rest)


total<-nrow(EventProbs)
totals<-rbind(totals,total)
}

Groom<-Grooms %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Grooms))
Groom$Var<-'Groom'

Travel<-Travels %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Travels))
Travel$Var<-'Travel'
Forage<-Forages %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Forages))
Forage$Var<-'Forage'
Rest<-Rests %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Rests))
Rest$Var<-'Rest'

ConfMat<-rbind(Groom, Rest, Travel, Forage)
write.csv(ConfMat, "output/summaries/Wild_confusion_matrix.csv")

(sum(totals)-sum(nrow(Groom),nrow(Rest),nrow(Travel),nrow(Forage)))/sum(totals)

