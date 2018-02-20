split_data<-function(acc,i){

source("r/functions.R")
source("r/1Hz_models/add_place_run_1Hz.R")

library(ggplot2)
library(hydromad)
library(lubridate)
library(qdapTools)
library(robfilter)
library(signal)

  SealFiles <- list.files(paste0("data/wilddata_1Hz/"))
      
  acc$time<-as.numeric(seconds(acc$date))
  
    #  load the wet/dry data
  load(paste0("output/wetdry_times/",j))
  
  ###find start time
  #use start and end time set manually
  #seal<-substr(SealFiles[j],1,(nchar(SealFiles[j])-6))
  
  start.time<-lookup(seal,deets[,c("seal","start.date")])
  end.time<-lookup(seal,deets[,c("seal","end.date")])
  
  ##clean 1Hz file (remove data before acc was on seal)
  acc<-acc[as.numeric(acc$time)>start.time&as.numeric(acc$time)<end.time,]
  
  #file must start after wet/dry starts
  start.time<-wet_dry_times[1,start_time]
  start<-acc[1,"time"]
  
  if(start.time>start){
    acc<-acc[acc$time>start.time,]
  }
  #testing
  #acc<-acc[50000:nrow(acc),]
  
  ##add place to files
  acc<-add_place_1Hz(acc,j)  
  acc$location<-acc$Place
  
  ###smooth the depth data
  DepthSmooth<-data.frame(runmed(acc$depth,7))
  acc$DepthSmooth<-as.numeric(unlist(DepthSmooth))
  
  acc$Place<-ifelse(acc$depth>2.5,"underwater",ifelse(acc$location=="dry","land","surface"))
  acc$Place<-factor(acc$Place)
  acc$numPlace<-as.numeric(acc$Place)
  
  acc$nPx<-as.numeric(runmed(acc$numPlace,7))
  xx<-data.frame(unclass(rle(acc$nPx)))
 
  min(xx$lengths)
  max(xx$lengths)
  
  acc$numEvents<-rep(1:length(xx$values),xx$lengths)
  acc$nPx<-factor(acc$nPx,labels = c("land","surface","underwater"))
 
  ##define events by depth
  #x<-hydromad::eventseq(acc$DepthSmooth,thresh = 3,inthresh = 1, indur = 3,mindur = 10,continue = TRUE, below = TRUE)
  #y<-as.data.frame(table(coredata(x)))

  #acc$EventIds<-rep(1:length(y$Var1),y$Freq)
  #acc$eventsLoc<-paste0(acc$EventIds,".",acc$nPx)
  #xx<-data.frame(unclass(rle(acc$nPx)))
  
  #Events<-unique(acc$numEvents)

#Calculate ODBA and VeDBA of the raw data
  #running mean
    acc$xma<-runmean(acc$x,10)       #the running mean is based on the best model
    acc$yma<-runmean(acc$y,10)       #from the energetics paper
    acc$zma<-runmean(acc$z,10)  
  #dynamic mean
    acc$dx<-acc$x-acc$xma
    acc$dy<-acc$y-acc$yma
    acc$dz<-acc$z-acc$zma 
  #ODBA and VeDBA
    acc$ODBA <- (abs(acc$dx)+abs(acc$dy)+abs(acc$dz))
    acc$VeDBA <- sqrt((acc$dx)^2+(acc$dy)^2+(acc$dz)^2)

#threshold<-0.2
##apply the threshold
#dba<-acc[acc$VeDBA>threshold,]

###ODBA and VeDBA AUC
#output<-NULL

#for(i in 1:length(Events)){
 # dat<-dba[dba$eventNum==Events[i],]
#  ODBA.auc   <- MESS::auc(dat$date,   dat$ODBA) 
#  VeDBA.auc   <- MESS::auc(dat$date,   dat$VeDBA)  
#  EnergyEx <- -12.62+3.8*VeDBA.auc
#  new.dat<-cbind(Events[i],VeDBA.auc,ODBA.auc,EnergyEx)
#  output<-rbind(output,new.dat)
#}
    return(acc)

} #end function
