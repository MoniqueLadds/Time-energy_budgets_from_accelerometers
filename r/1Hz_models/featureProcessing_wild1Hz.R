featureProcessing <- function(epochs){
  library(e1071)  
  library("circular", quietly=TRUE)
  library(MESS)
  library(reshape2)
  require(caTools)
  library(pracma)
  library(lubridate)
  library(qdapTools)
    
  source("r/functions.R")
  source('r/1Hz_models/add_place_run_1Hz.R')
  
  
    fileLoc <- paste0("data/wilddata_1Hz/")
      
    SealNames <- list.files(fileLoc)
    
    deets<-read.csv("start.end_times.csv",stringsAsFactors=FALSE)
    deets$start.date<-as.POSIXct(deets$start.date,format="%d/%m/%Y %H:%M")
    deets$end.date<-as.POSIXct(deets$end.date,format="%d/%m/%Y %H:%M")
    deets$start.secs<-as.numeric(seconds(deets$start.date))
    deets$end.secs<-as.numeric(seconds(deets$end.date))
    
  
  outputData <- NULL

    for(j in 1:length(SealNames)){
     
      ##### read in the data
      cat(paste(fileLoc,
                SealNames[j],
                sep="/"),
          "\n")
      
      
      j=j
	    #epochs=7
      load(paste(fileLoc, SealNames[j], sep="/"))
      acc$time<-as.numeric(seconds(acc$date))
      
      #  load the wet/dry data
      load(paste0("output/wetdry_times/",SealNames[j]))
      
      ###find start time
      #use start and end time set manually
      seal<-substr(SealNames[j],1,(nchar(SealNames[j])-6))
      
      start.time<-lookup(seal,deets[,c("id","start.secs")])
      end.time<-lookup(seal,deets[,c("id","end.secs")])
      
      ##clean 1Hz file (remove data before acc was on seal)
      acc<-acc[as.numeric(acc$time)>start.time&as.numeric(acc$time)<end.time,]
      
      #file must start after wet/dry starts
      start.time<-wet_dry_times[1,start_time]
      start<-acc[1,"time"]
      
      if(start.time>start){
        acc<-acc[acc$time>start.time,]
      }
      
      acc<-add_place_1Hz(acc,j)   
      acc$location<-acc$Place
      
      acc$Place<-ifelse(acc$depth>2.5,"underwater",ifelse(acc$location=="dry","land","surface"))
      acc$Place<-factor(acc$Place)
      
     # if(endSample){
      #output<-as.data.frame(acc[1:endSample,])
      #}else{
        output<-as.data.frame(acc)
      #}
      
      
      rm(acc)
      ####################################################
      ####################################################
      output$sealName <- substr(SealNames[j],8,(nchar(SealNames[j])-6))
     
      ###choose epoch length by which to summarise features
      epoch<-rep(1:(nrow(output)/epochs), each=epochs)
      output<-output[1:length(epoch),]
      output$epoch<-epoch    
      
      output$EventIds<-(paste0(substr(SealNames[j],1,(nchar(SealNames[j])-12)),"_",epoch))
      
      #filter x, y, z for noise
      #output$x<-HampelFilter(output$x, k=30)$y
      
      #output$y<-hampel(output$y, k=3)$y
      
      #output$z<-hampel(output$z, k=3)$y
      
      #Calculate ODBA and VeDBA
      output$grav.x<-runmean(output$x,3)
      output$grav.y<-runmean(output$y,3)
      output$grav.z<-runmean(output$z,3)  
      output$DynODBA.x<-output$x-output$grav.x
      output$DynODBA.y<-output$y-output$grav.y
      output$DynODBA.z<-output$z-output$grav.z 
      
      output$ODBA <- (abs(output$DynODBA.x)+abs(output$DynODBA.y)+abs(output$DynODBA.y))
      output$VeDBA <- sqrt((output$DynODBA.x)^2+(output$DynODBA.y)^2+(output$DynODBA.y)^2)
      
      ##From Alvarenga
      output$sma<-abs(output$x)+abs(output$y)+abs(output$z)
      output$svm<-sqrt(output$x^2+output$y^2+output$z^2)
      output$energy<-(output$x^2+output$y^2+output$z^2)^2
      output$entropy<-(1+(output$x+output$y+output$z)^2)*log(1+(output$x+output$y+output$z)^2)
      #output$pitch<-tan(-output$x/(sqrt(output$y+output$z)))*180/pi
      output$roll<-atan2(output$y,output$z)*180/pi
      #output$inclination<-tan(sqrt(output$x+output$y)/output$z)*180/pi
      
      movvar<-abs(diff(output$x,lag=1))+abs(diff(output$y,lag=1))+abs(diff(output$z,lag=1))
      output<-output[-1,]
      output$movvar<-movvar
      
      #### data storage
      featureData <- NULL
      
      ##### summarise some shit
      featureData$pdbax<-tapply(output$grav.x,output$EventIds,mean)
      featureData$pdbay<-tapply(output$grav.y,output$EventIds,mean)
      featureData$pdbaz<-tapply(output$grav.z,output$EventIds,mean)
      featureData$dx<-tapply(output$DynODBA.x,output$EventIds,mean)
      featureData$dy<-tapply(output$DynODBA.y,output$EventIds,mean)
      featureData$dz<-tapply(output$DynODBA.z,output$EventIds,mean)
      
      ###calculate ODBA
      featureData$ODBA.mean<-tapply(output$ODBA,output$EventIds,mean)
      ###calculate VeDBA      
      featureData$VeDBA.mean<-tapply(output$VeDBA,output$EventIds,mean)
      
      
      ##make variables for summary file###
      #All x summaries
      featureData$eventMean.x <- tapply(output$x, output$EventIds, mean)
      featureData$eventSd.x <-tapply(output$x,output$EventIds,sd.default)
      featureData$eventAbs.x <- tapply(abs(output$DynODBA.x), output$EventIds, mean)
      featureData$eventSkew.x <- tapply(output$x,output$EventIds,skewness)
      featureData$eventKurt.x <- tapply(output$x,output$EventIds,kurtosis)
      featureData$eventMax.x <- tapply(output$x,output$EventIds,max)
      featureData$eventMin.x <- tapply(output$x,output$EventIds,min)
      featureData$eventMed.x <- tapply(output$x,output$EventIds,median)
      featureData$eventMad.x <- tapply(output$x,output$EventIds,mad)
      featureData$InvCovar.x <- featureData$eventSd.x / featureData$eventMean.x
      
      
      #All Y summaries
      featureData$eventMean.y <- tapply(output$y, output$EventIds, mean)
      featureData$eventSd.y <-tapply(output$y,output$EventIds,sd.default)
      featureData$eventAbs.y <- tapply(abs(output$DynODBA.y), output$EventIds, mean)
      featureData$eventSkew.y <- tapply(output$y,output$EventIds,skewness)
      featureData$eventKurt.y <- tapply(output$y,output$EventIds,kurtosis)
      featureData$eventMax.y <- tapply(output$y,output$EventIds,max)
      featureData$eventMin.y <- tapply(output$y,output$EventIds,min)
      featureData$eventMed.y <- tapply(output$y,output$EventIds,median)
      featureData$eventMad.y <- tapply(output$y,output$EventIds,mad)
      featureData$InvCovar.y <- featureData$eventSd.y / featureData$eventMean.y
      
      
      
      #All Z summaries
      featureData$eventMean.z <- tapply(output$z, output$EventIds, mean)
      featureData$eventSd.z <-tapply(output$z,output$EventIds,sd.default)
      featureData$eventAbs.z <- tapply(abs(output$DynODBA.z), output$EventIds, mean)
      featureData$eventSkew.z <- tapply(output$z,output$EventIds,skewness)
      featureData$eventKurt.z <- tapply(output$z,output$EventIds,kurtosis)
      featureData$eventMax.z <- tapply(output$z,output$EventIds,max)
      featureData$eventMin.z <- tapply(output$z,output$EventIds,min)
      featureData$eventMed.z <- tapply(output$z,output$EventIds,median)
      featureData$eventMad.z <- tapply(output$z,output$EventIds,mad)
      featureData$InvCovar.z <- featureData$eventSd.z / featureData$eventMean.z
      
      ###### obtaining event names in this way preserves the ordering (unique is reordering the variables)
      Events <- names(featureData$eventMad.z)
      nEvents <- length(Events)
      
      Ids <- list(rep.int(SealNames[j], length(Events)))
      
      ###ODBA and VeDBA AUC
      outputs<-NULL
      
      for(i in 1:length(Events)){
        dat<-output[output$EventIds==Events[i],]
        ODBA.auc   <- MESS::auc(dat$date,   dat$ODBA) 
        VeDBA.auc   <- MESS::auc(dat$date,   dat$VeDBA)  
        new.dat<-cbind(VeDBA.auc,ODBA.auc)
        outputs<-rbind(outputs,new.dat)
      }
      
      
      #Variable Q
      output$ss<-sqrt((output$x)^2+(output$y)^2+(output$z)^2)
      featureData$Qstat <- tapply(output$ss,output$EventIds,mean)
      
      ##Trend value
      #is the coefficient from the linear regression between an axis (xyz) 
      #and time? Yes
      # not sure if you want an intercept here? probably do, but one is not included yet
      
      featureData$trend.x <- tapply(output$x,output$EventIds, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      featureData$trend.y <- tapply(output$y,output$EventIds, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      featureData$trend.z <- tapply(output$z,output$EventIds, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      #Make new variables for summaries
      #not sure how to summarise these yet...  
      # you nee to calculae a circular variance
      # browser()
      featureData$inclination.cir <- tapply(acos(output$z/output$ss),output$EventIds, circular::var)
      featureData$azimuth.cir <- tapply(atan(output$y/output$z),output$EventIds, var, na.rm=T)
      
      
      #       browser()
      SealName <-  substr(SealNames[j],8,(nchar(SealNames[j])-6))
      featureData$SealName <- SealName
      #featureData$FileDate <- substr(FileNames[k],1,8)
      #featureData$behaviour<-tapply(output$behav_levels,as.factor(output$EventIds),Mode)
      #featureData$nRows <- tapply(output$EventIds, output$EventIds, length)
      
      featureData <- data.frame(featureData)
      featureData <- cbind(featureData,outputs)
      
      ##add place
      uPlace <- unique(output$Place)
     
      if(length(uPlace) > 1){
        Place <- dcast(data = output,
                       formula = EventIds ~ Place, fun.aggregate = length, value.var = "Place")[1:length(uPlace)+1]
       labels <- names(Place)
        
       featureData$Place <- labels[max.col(Place)]
        
      }else{
        featureData$Place <- uPlace
        
      }
 
      cat(nrow(featureData),"\n")
     save(featureData,
		file=paste0("output/wild_feature_1Hz/outputData_",SealName,epochs,".RData")) 


}   ####end loop

 }   ###end function