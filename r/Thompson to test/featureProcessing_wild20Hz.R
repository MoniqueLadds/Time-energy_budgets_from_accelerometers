featureProcessing <- function(epochs,endSample){
  library(e1071)  
  library("circular", quietly=TRUE)
  library(MESS)
  library(reshape2)
  require(caTools)
  library(pracma)
  source("r/functions.R")
  
     fileLoc <- paste0("data/wilddata")
      
    SealNames <- list.files(fileLoc)
  #for PC
  #SealNames <- SealNames[!grepl("desktop.ini", SealNames)]
  
  
  outputData <- NULL

    for(j in 1:length(SealNames)){
     
      ##### read in the data
      cat(paste(fileLoc,
                SealNames[j],
                sep="/"),
          "\n")
      
      
      #j=1
	    #epochs=75
      load(paste(fileLoc, SealNames[j], sep="/"))
      
      if(endSample){
      df<-df[1:endSample,]
      }   
          
      df$Place<-ifelse(df$depth>2.5,"underwater","surface")
      
      dives<-unique(df$divenum)
      
      ##loop over dives
      
      for(k in 1:length(dives)){
        
      output<-as.data.frame(df[df$divenum==dives[k],])
    
      cat(dives[k],"\n")
      ####################################################
      ####################################################
      output$sealName <- substr(SealNames[j],8,(nchar(SealNames[j])-6))
     
      ###choose epoch length by which to summarise features
      if(nrow(output)>epochs){
      epoch<-rep(1:(nrow(output)/epochs), each=epochs)
      output<-output[1:length(epoch),]
      output$epoch<-epoch   
      }else{
        epoch<-rep(1,nrow(output))
        output$epoch<-1 
      }
      
      output$EventIds<-(paste0(epoch,"_",substr(SealNames[j],1,(nchar(SealNames[j])-14))))
      
      #filter x, y, z for noise
      #output$x<-HampelFilter(output$x, k=30)$y
      
      #output$y<-hampel(output$y, k=3)$y
      
      #output$z<-hampel(output$z, k=3)$y
      
      #Calculate ODBA and VeDBA
      output$xma<-runmean(output$x,40)
      output$yma<-runmean(output$y,40)
      output$zma<-runmean(output$z,40)  
      output$dx<-output$x-output$xma
      output$dy<-output$y-output$yma
      output$dz<-output$z-output$zma 
      
      output$ODBA <- (abs(output$dx)+abs(output$dy)+abs(output$dz))
      output$VeDBA <- sqrt((output$dx)^2+(output$dy)^2+(output$dz)^2)
      
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
      featureData$pdbax<-tapply(output$xma,output$EventIds,mean)
      featureData$pdbay<-tapply(output$yma,output$EventIds,mean)
      featureData$pdbaz<-tapply(output$zma,output$EventIds,mean)
      featureData$dx<-tapply(output$dx,output$EventIds,mean)
      featureData$dy<-tapply(output$dy,output$EventIds,mean)
      featureData$dz<-tapply(output$dz,output$EventIds,mean)
      
      featureData$pdbaxMin<-tapply(output$xma,output$EventIds,min)
      featureData$pdbayMin <-tapply(output$yma,output$EventIds,min)
      featureData$pdbazMin <-tapply(output$zma,output$EventIds,min)
      featureData$dxMin <-tapply(output$dx,output$EventIds,min)
      featureData$dyMin <-tapply(output$dy,output$EventIds,min)
      featureData$dzMin <-tapply(output$dz,output$EventIds,min)
      
      featureData$pdbaxMax <-tapply(output$xma,output$EventIds,max)
      featureData$pdbayMax <-tapply(output$yma,output$EventIds,max)
      featureData$pdbazMax <-tapply(output$zma,output$EventIds,max)
      featureData$dxMax <-tapply(output$dx,output$EventIds,max)
      featureData$dyMax <-tapply(output$dy,output$EventIds,max)
      featureData$dzMax <-tapply(output$dz,output$EventIds,max)
      
      featureData$pdbaxLQ<-tapply(output$xma,output$EventIds, quantile, 0.25)
      featureData$pdbayLQ <-tapply(output$yma,output$EventIds, quantile, 0.25)
      featureData$pdbazLQ <-tapply(output$zma,output$EventIds, quantile, 0.25)
      featureData$dxLQ <-tapply(output$dx,output$EventIds,quantile, 0.25)
      featureData$dyLQ <-tapply(output$dy,output$EventIds,quantile, 0.25)
      featureData$dzLQ <-tapply(output$dz,output$EventIds,quantile, 0.25)
      
      featureData$pdbaxUQ<-tapply(output$xma,output$EventIds, quantile, 0.75)
      featureData$pdbayUQ <-tapply(output$yma,output$EventIds, quantile, 0.75)
      featureData$pdbazUQ <-tapply(output$zma,output$EventIds, quantile, 0.75)
      featureData$dxUQ <-tapply(output$dx,output$EventIds,quantile, 0.75)
      featureData$dyUQ <-tapply(output$dy,output$EventIds,quantile, 0.75)
      featureData$dzUQ <-tapply(output$dz,output$EventIds,quantile, 0.75)
      
      featureData$pdbax10<-tapply(output$xma,output$EventIds, quantile, 0.10)
      featureData$pdbay10 <-tapply(output$yma,output$EventIds, quantile, 0.10)
      featureData$pdbaz10 <-tapply(output$zma,output$EventIds, quantile, 0.10)
      featureData$pdbax90<-tapply(output$xma,output$EventIds, quantile, 0.90)
      featureData$pdbay90 <-tapply(output$yma,output$EventIds, quantile, 0.90)
      featureData$pdbaz90 <-tapply(output$zma,output$EventIds, quantile, 0.90)
      featureData$dx10 <-tapply(output$dx,output$EventIds,quantile, 0.10)
      featureData$dy10 <-tapply(output$dy,output$EventIds,quantile, 0.10)
      featureData$dz10 <-tapply(output$dz,output$EventIds,quantile, 0.10)
      featureData$dx90 <-tapply(output$dx,output$EventIds,quantile, 0.90)
      featureData$dy90 <-tapply(output$dy,output$EventIds,quantile, 0.90)
      featureData$dz90 <-tapply(output$dz,output$EventIds,quantile, 0.90)
      
      featureData$dz90R10 <- featureData$dz90 - featureData$dz10
      featureData$dx90R10 <- featureData$dx90 - featureData$dx10
      featureData$dy90R10 <- featureData$dy90 - featureData$dy10
      
      featureData$pdbax90R10 <- featureData$pdbax90 - featureData$pdbax10
      featureData$pdbay90R10 <- featureData$pdbay90 - featureData$pdbay10
      featureData$pdbaz90R10 <- featureData$pdbaz90 - featureData$pdbaz10
      
      featureData$dzRange <- featureData$dzMax - featureData$dzMin
      featureData$dxRange <- featureData$dxMax - featureData$dxMin
      featureData$dyRange <- featureData$dyMax - featureData$dyMin
      
      featureData$pdbaxRange <- featureData$pdbaxMax - featureData$pdbaxMin
      featureData$pdbayRange <- featureData$pdbayMax - featureData$pdbayMin
      featureData$pdbazRange <- featureData$pdbazMax - featureData$pdbazMin
      
      ###calculate ODBA
      featureData$odba <- tapply(output$ODBA,
                                 output$EventIds,
                                 mean)
      featureData$odbaMAX <- tapply(output$ODBA,
                                    output$EventIds,
                                    max)
      featureData$odbaMin <- tapply(output$ODBA,
                                    output$EventIds,
                                    min)
      featureData$odbaUQ <- tapply(output$ODBA,
                                   output$EventIds,
                                   quantile, 0.75)
      featureData$odbaLQ <- tapply(output$ODBA,
                                   output$EventIds,
                                   quantile, 0.25)
      featureData$odbaIQR <- featureData$odbaUQ - featureData$odbaLQ
      featureData$odbaMedian <- tapply(output$ODBA,
                                       output$EventIds,
                                       median)
      featureData$odbaSD <- tapply(output$ODBA,
                                   output$EventIds,
                                   sd)
      featureData$odba10 <- tapply(output$ODBA,
                                   output$EventIds,
                                   quantile, 0.10)
      featureData$odba90 <- tapply(output$ODBA,
                                   output$EventIds,
                                   quantile, 0.90)
      featureData$odba90R10 <- featureData$odba90 - featureData$odba10
      featureData$odbaRange <- featureData$odbaMAX - featureData$odbaMin
      
      ###calculate VeDBA      
      featureData$VeDBA.mean<-tapply(output$VeDBA,
                                     output$EventIds,
                                     mean)
      featureData$VeDBAMAX <- tapply(output$VeDBA,
                                     output$EventIds,
                                     max)
      featureData$VeDBAMin <- tapply(output$VeDBA,
                                     output$EventIds,
                                     min)
      featureData$VeDBAUQ <- tapply(output$VeDBA,
                                    output$EventIds,
                                    quantile, 0.75)
      featureData$VeDBALQ <- tapply(output$VeDBA,
                                    output$EventIds,
                                    quantile, 0.25)
      featureData$VeDBAIQR <- featureData$VeDBAUQ - featureData$VeDBALQ
      featureData$VeDBAMedian <- tapply(output$VeDBA,
                                        output$EventIds,
                                        median)
      featureData$VeDBASD <- tapply(output$VeDBA,
                                    output$EventIds,
                                    sd)
      featureData$VeDBA10 <- tapply(output$VeDBA,
                                    output$EventIds,
                                    quantile, 0.10)
      featureData$VeDBA90 <- tapply(output$VeDBA,
                                    output$EventIds,
                                    quantile, 0.90)
      featureData$VeDBA90R10 <- featureData$VeDBA90 - featureData$VeDBA10
      featureData$VeDBARange <- featureData$VeDBAMAX - featureData$VeDBAMin
      
      ##make variables for summary file###
      #All x summaries
      featureData$eventMean.x <- tapply(output$x, output$EventIds, mean)
      featureData$eventSd.x <-tapply(output$x,output$EventIds,sd.default)
      featureData$eventAbs.x <- tapply(abs(output$dx), output$EventIds, mean)
      featureData$eventSkew.x <- tapply(output$x,output$EventIds,skewness)
      featureData$eventKurt.x <- tapply(output$x,output$EventIds,kurtosis)
      featureData$eventMax.x <- tapply(output$x,output$EventIds,max)
      featureData$eventMin.x <- tapply(output$x,output$EventIds,min)
      featureData$eventRange.x <- featureData$eventMax.x - featureData$eventMin.x 
      featureData$eventMed.x <- tapply(output$x,output$EventIds,median)
      featureData$eventMad.x <- tapply(output$x,output$EventIds,mad)
      featureData$InvCovar.x <- featureData$eventSd.x / featureData$eventMean.x
      featureData$eventLQ.x <- tapply(output$x,output$EventIds, quantile, 0.25)
      featureData$eventUQ.x <- tapply(output$x,output$EventIds, quantile, 0.75)
      featureData$event10.x <- tapply(output$x,output$EventIds, quantile, 0.10)
      featureData$event90.x <- tapply(output$x,output$EventIds, quantile, 0.90)
      featureData$event90R10.x <- featureData$event90.x - featureData$event10.x
      featureData$eventIQR.x <- featureData$eventUQ.x - featureData$eventLQ.x
      
      #All Y summaries
      featureData$eventMean.y <- tapply(output$y, output$EventIds, mean)
      featureData$eventSd.y <-tapply(output$y,output$EventIds,sd.default)
      featureData$eventAbs.y <- tapply(abs(output$dy), output$EventIds, mean)
      featureData$eventSkew.y <- tapply(output$y,output$EventIds,skewness)
      featureData$eventKurt.y <- tapply(output$y,output$EventIds,kurtosis)
      featureData$eventMax.y <- tapply(output$y,output$EventIds,max)
      featureData$eventMin.y <- tapply(output$y,output$EventIds,min)
      featureData$eventRange.y <- featureData$eventMax.y - featureData$eventMin.y 
      featureData$eventMed.y <- tapply(output$y,output$EventIds,median)
      featureData$eventMad.y <- tapply(output$y,output$EventIds,mad)
      featureData$InvCovar.y <- featureData$eventSd.y / featureData$eventMean.y
      featureData$eventLQ.y <- tapply(output$y,output$EventIds, quantile, 0.25)
      featureData$eventUQ.y <- tapply(output$y,output$EventIds, quantile, 0.75)
      featureData$event10.y <- tapply(output$y,output$EventIds, quantile, 0.10)
      featureData$event90.y <- tapply(output$y,output$EventIds, quantile, 0.90)
      featureData$event90R10.y <- featureData$event90.y - featureData$event10.y
      featureData$eventIQR.y <- featureData$eventUQ.y - featureData$eventLQ.y
      
      #All Z summaries
      featureData$eventMean.z <- tapply(output$z, output$EventIds, mean)
      featureData$eventSd.z <-tapply(output$z,output$EventIds,sd.default)
      featureData$eventAbs.z <- tapply(abs(output$dz), output$EventIds, mean)
      featureData$eventSkew.z <- tapply(output$z,output$EventIds,skewness)
      featureData$eventKurt.z <- tapply(output$z,output$EventIds,kurtosis)
      featureData$eventMax.z <- tapply(output$z,output$EventIds,max)
      featureData$eventMin.z <- tapply(output$z,output$EventIds,min)
      featureData$eventRange.z <- featureData$eventMax.z - featureData$eventMin.z
      featureData$eventMed.z <- tapply(output$z,output$EventIds,median)
      featureData$eventMad.z <- tapply(output$z,output$EventIds,mad)
      featureData$InvCovar.z <- featureData$eventSd.z / featureData$eventMean.z
      featureData$eventLQ.z <- tapply(output$z,output$EventIds, quantile, 0.25)
      featureData$eventUQ.z <- tapply(output$z,output$EventIds, quantile, 0.75)
      featureData$event10.z <- tapply(output$z,output$EventIds, quantile, 0.10)
      featureData$event90.z <- tapply(output$z,output$EventIds, quantile, 0.90)
      featureData$event90R10.z <- featureData$event90.z - featureData$event10.z
      featureData$eventIQR.z <- featureData$eventUQ.z - featureData$eventLQ.z
      
      ###### obtaining event names in this way preserves the ordering (unique is reordering the variables)
      Events <- names(featureData$eventMad.z)
      nEvents <- length(Events)
      
      Ids <- list(rep.int(SealNames[j], length(Events)))
      
      ###ODBA and VeDBA AUC
      outputs<-NULL
      
      for(i in 1:length(Events)){
        dat<-output[output$EventIds==Events[i],]
        ODBA.auc   <- MESS::auc(dat$time,   dat$ODBA) 
        VeDBA.auc   <- MESS::auc(dat$time,   dat$VeDBA)  
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
    
      
      ###calculating correlations
      #featureData$Acf.x <- matrix(0,nrow=nEvents,ncol=1)    
      #featureData$Acf.y <- matrix(0,nrow=nEvents,ncol=1)
      #featureData$Acf.z <- matrix(0,nrow=nEvents,ncol=1)
      #featureData$Corr.xy <- matrix(0,nrow=nEvents,ncol=1)
      #featureData$Corr.xz <- matrix(0,nrow=nEvents,ncol=1)
      #featureData$Corr.yz <- matrix(0,nrow=nEvents,ncol=1)
      
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
      
      ##add time
     # uTime <- unique(output$time_stamp)
      
      #if(length(uTime) > 1){
      #  time_stamp <- dcast(data = output,
       #                formula = EventIds ~ time_stamp, fun.aggregate = length, value.var = "time_stamp")[1:length(uTime)+1]
        #labels <- names(time_stamp)
        
        #featureData$time_stamp <- labels[max.col(time_stamp)]
        
    #  }else{
     #   featureData$time_stamp <- uTime
        
      #}
      
      outputData<-rbind(outputData,featureData)
      }##end k
 
      #cat(nrow(featureData),"\n")
     save(outputData,
		file=paste0("output/wild_feature_20Hz/outputData_",SealName,epochs,".RData")) 


}   ####end loop [j]

 }   ###end function