featureProcessing <- function(epochs,family){
  library(e1071)  
  library("circular", quietly=TRUE)
  library(MESS)
  library(reshape2)
  require(caTools)
  library(pracma)
  source("r/functions.R")
  
     fileLoc <- paste0("data/",family,"/behaviour_20Hz/")
      
    SealNames <- list.files(fileLoc)
  #for PC
  SealNames <- SealNames[!grepl("desktop.ini", SealNames)]
  
  
  outputData <- NULL
  for(j in 1:length(SealNames)){
    FileNames <- list.files(paste(fileLoc, SealNames[j], sep="/"))
    FileNames <- FileNames[!grepl("desktop.ini", FileNames)]
    FileNames <- FileNames[!grepl("Thumbs.db", FileNames)]
    nFileNames <- length(FileNames)
    
    cat("j=",j,"\n")
    for(k in 1:nFileNames){
      cat("k=",k,"\n")
      
          
      ##### read in the data
      cat(paste(fileLoc,
                SealNames[j],
                FileNames[k],
                sep="/"),
          "\n")
      
      
      output<-read.csv(paste(fileLoc, SealNames[j], FileNames[k],  sep="/"),
                       stringsAsFactors=FALSE)
      
      table(output$type,output$place)
      ####################################################
      ####################################################
       # browser()
      op <- options(digits.secs = 2)
      # options(op) #using this seems to return to the default behaviour of no decimal seconds
      output$time <- as.POSIXct(output$time, format="%Y-%m-%d %H:%M:%OS")
      output$date <- substr(FileNames[k],start = 1, stop = 8)
      #reduce number of columns      
      output <- output[,c("time","date","type","type_event","x","y","z","behaviour","place","behaviour_event")]
      output$sealName <- SealNames[j]
      
      ##remove duplicated times
      #output<-output[!duplicated(output[,c(1:6)]),]
      
      ###remove behaviours that occur for less than 3 seconds    
      output<-output[unsplit(table(output$type_event), output$type_event) >= 3, ]
     
      
      ###choose epoch_event length by which to summarise features
      epoch<-rep(1:(nrow(output)/epochs), each=epochs)
      output<-output[1:length(epoch),]
      output$epoch<-epoch    
      
      output$epoch_event<-paste0(output$epoch,".",output$type)
      output$type_event<-(as.factor(output$type_event))
      output$type<-(as.factor(output$type))
      output$behav_levels<-as.numeric(output$type)
      
      #filter x, y, z for noise
      output$x<-hampel(output$x, k=3)$y
      
      output$y<-hampel(output$y, k=3)$y
      
      output$z<-hampel(output$z, k=3)$y
      
      #Calculate ODBA and VeDBA
      output$xma<-runmean(output$x,3)
      output$yma<-runmean(output$y,3)
      output$zma<-runmean(output$z,3)  
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
      featureData$pdbax<-tapply(output$xma,output$epoch_event,mean)
      featureData$pdbay<-tapply(output$yma,output$epoch_event,mean)
      featureData$pdbaz<-tapply(output$zma,output$epoch_event,mean)
      featureData$dx<-tapply(output$dx,output$epoch_event,mean)
      featureData$dy<-tapply(output$dy,output$epoch_event,mean)
      featureData$dz<-tapply(output$dz,output$epoch_event,mean)
      
      featureData$pdbaxMin<-tapply(output$xma,output$epoch_event,min)
      featureData$pdbayMin <-tapply(output$yma,output$epoch_event,min)
      featureData$pdbazMin <-tapply(output$zma,output$epoch_event,min)
      featureData$dxMin <-tapply(output$dx,output$epoch_event,min)
      featureData$dyMin <-tapply(output$dy,output$epoch_event,min)
      featureData$dzMin <-tapply(output$dz,output$epoch_event,min)
      
      featureData$pdbaxMax <-tapply(output$xma,output$epoch_event,max)
      featureData$pdbayMax <-tapply(output$yma,output$epoch_event,max)
      featureData$pdbazMax <-tapply(output$zma,output$epoch_event,max)
      featureData$dxMax <-tapply(output$dx,output$epoch_event,max)
      featureData$dyMax <-tapply(output$dy,output$epoch_event,max)
      featureData$dzMax <-tapply(output$dz,output$epoch_event,max)
      
      featureData$pdbaxLQ<-tapply(output$xma,output$epoch_event, quantile, 0.25)
      featureData$pdbayLQ <-tapply(output$yma,output$epoch_event, quantile, 0.25)
      featureData$pdbazLQ <-tapply(output$zma,output$epoch_event, quantile, 0.25)
      featureData$dxLQ <-tapply(output$dx,output$epoch_event,quantile, 0.25)
      featureData$dyLQ <-tapply(output$dy,output$epoch_event,quantile, 0.25)
      featureData$dzLQ <-tapply(output$dz,output$epoch_event,quantile, 0.25)
      
      featureData$pdbaxUQ<-tapply(output$xma,output$epoch_event, quantile, 0.75)
      featureData$pdbayUQ <-tapply(output$yma,output$epoch_event, quantile, 0.75)
      featureData$pdbazUQ <-tapply(output$zma,output$epoch_event, quantile, 0.75)
      featureData$dxUQ <-tapply(output$dx,output$epoch_event,quantile, 0.75)
      featureData$dyUQ <-tapply(output$dy,output$epoch_event,quantile, 0.75)
      featureData$dzUQ <-tapply(output$dz,output$epoch_event,quantile, 0.75)
      
      featureData$pdbax10<-tapply(output$xma,output$epoch_event, quantile, 0.10)
      featureData$pdbay10 <-tapply(output$yma,output$epoch_event, quantile, 0.10)
      featureData$pdbaz10 <-tapply(output$zma,output$epoch_event, quantile, 0.10)
      featureData$pdbax90<-tapply(output$xma,output$epoch_event, quantile, 0.90)
      featureData$pdbay90 <-tapply(output$yma,output$epoch_event, quantile, 0.90)
      featureData$pdbaz90 <-tapply(output$zma,output$epoch_event, quantile, 0.90)
      featureData$dx10 <-tapply(output$dx,output$epoch_event,quantile, 0.10)
      featureData$dy10 <-tapply(output$dy,output$epoch_event,quantile, 0.10)
      featureData$dz10 <-tapply(output$dz,output$epoch_event,quantile, 0.10)
      featureData$dx90 <-tapply(output$dx,output$epoch_event,quantile, 0.90)
      featureData$dy90 <-tapply(output$dy,output$epoch_event,quantile, 0.90)
      featureData$dz90 <-tapply(output$dz,output$epoch_event,quantile, 0.90)
      
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
                                 output$epoch_event,
                                 mean)
      featureData$odbaMAX <- tapply(output$ODBA,
                                    output$epoch_event,
                                    max)
      featureData$odbaMin <- tapply(output$ODBA,
                                    output$epoch_event,
                                    min)
      featureData$odbaUQ <- tapply(output$ODBA,
                                   output$epoch_event,
                                   quantile, 0.75)
      featureData$odbaLQ <- tapply(output$ODBA,
                                   output$epoch_event,
                                   quantile, 0.25)
      featureData$odbaIQR <- featureData$odbaUQ - featureData$odbaLQ
      featureData$odbaMedian <- tapply(output$ODBA,
                                       output$epoch_event,
                                       median)
      featureData$odbaSD <- tapply(output$ODBA,
                                   output$epoch_event,
                                   sd)
      featureData$odba10 <- tapply(output$ODBA,
                                   output$epoch_event,
                                   quantile, 0.10)
      featureData$odba90 <- tapply(output$ODBA,
                                   output$epoch_event,
                                   quantile, 0.90)
      featureData$odba90R10 <- featureData$odba90 - featureData$odba10
      featureData$odbaRange <- featureData$odbaMAX - featureData$odbaMin
      
      ###calculate VeDBA      
      featureData$VeDBA.mean<-tapply(output$VeDBA,
                                     output$epoch_event,
                                     mean)
      featureData$VeDBAMAX <- tapply(output$VeDBA,
                                     output$epoch_event,
                                     max)
      featureData$VeDBAMin <- tapply(output$VeDBA,
                                     output$epoch_event,
                                     min)
      featureData$VeDBAUQ <- tapply(output$VeDBA,
                                    output$epoch_event,
                                    quantile, 0.75)
      featureData$VeDBALQ <- tapply(output$VeDBA,
                                    output$epoch_event,
                                    quantile, 0.25)
      featureData$VeDBAIQR <- featureData$VeDBAUQ - featureData$VeDBALQ
      featureData$VeDBAMedian <- tapply(output$VeDBA,
                                        output$epoch_event,
                                        median)
      featureData$VeDBASD <- tapply(output$VeDBA,
                                    output$epoch_event,
                                    sd)
      featureData$VeDBA10 <- tapply(output$VeDBA,
                                    output$epoch_event,
                                    quantile, 0.10)
      featureData$VeDBA90 <- tapply(output$VeDBA,
                                    output$epoch_event,
                                    quantile, 0.90)
      featureData$VeDBA90R10 <- featureData$VeDBA90 - featureData$VeDBA10
      featureData$VeDBARange <- featureData$VeDBAMAX - featureData$VeDBAMin
      
      ##make variables for summary file###
      #All x summaries
      featureData$eventMean.x <- tapply(output$x, output$epoch_event, mean)
      featureData$eventSd.x <-tapply(output$x,output$epoch_event,sd.default)
      featureData$eventAbs.x <- tapply(abs(output$dx), output$epoch_event, mean)
      featureData$eventSkew.x <- tapply(output$x,output$epoch_event,skewness)
      featureData$eventKurt.x <- tapply(output$x,output$epoch_event,kurtosis)
      featureData$eventMax.x <- tapply(output$x,output$epoch_event,max)
      featureData$eventMin.x <- tapply(output$x,output$epoch_event,min)
      featureData$eventRange.x <- featureData$eventMax.x - featureData$eventMin.x 
      featureData$eventMed.x <- tapply(output$x,output$epoch_event,median)
      featureData$eventMad.x <- tapply(output$x,output$epoch_event,mad)
      featureData$InvCovar.x <- featureData$eventSd.x / featureData$eventMean.x
      featureData$eventLQ.x <- tapply(output$x,output$epoch_event, quantile, 0.25)
      featureData$eventUQ.x <- tapply(output$x,output$epoch_event, quantile, 0.75)
      featureData$event10.x <- tapply(output$x,output$epoch_event, quantile, 0.10)
      featureData$event90.x <- tapply(output$x,output$epoch_event, quantile, 0.90)
      featureData$event90R10.x <- featureData$event90.x - featureData$event10.x
      featureData$eventIQR.x <- featureData$eventUQ.x - featureData$eventLQ.x
      
      #All Y summaries
      featureData$eventMean.y <- tapply(output$y, output$epoch_event, mean)
      featureData$eventSd.y <-tapply(output$y,output$epoch_event,sd.default)
      featureData$eventAbs.y <- tapply(abs(output$dy), output$epoch_event, mean)
      featureData$eventSkew.y <- tapply(output$y,output$epoch_event,skewness)
      featureData$eventKurt.y <- tapply(output$y,output$epoch_event,kurtosis)
      featureData$eventMax.y <- tapply(output$y,output$epoch_event,max)
      featureData$eventMin.y <- tapply(output$y,output$epoch_event,min)
      featureData$eventRange.y <- featureData$eventMax.y - featureData$eventMin.y 
      featureData$eventMed.y <- tapply(output$y,output$epoch_event,median)
      featureData$eventMad.y <- tapply(output$y,output$epoch_event,mad)
      featureData$InvCovar.y <- featureData$eventSd.y / featureData$eventMean.y
      featureData$eventLQ.y <- tapply(output$y,output$epoch_event, quantile, 0.25)
      featureData$eventUQ.y <- tapply(output$y,output$epoch_event, quantile, 0.75)
      featureData$event10.y <- tapply(output$y,output$epoch_event, quantile, 0.10)
      featureData$event90.y <- tapply(output$y,output$epoch_event, quantile, 0.90)
      featureData$event90R10.y <- featureData$event90.y - featureData$event10.y
      featureData$eventIQR.y <- featureData$eventUQ.y - featureData$eventLQ.y
      
      #All Z summaries
      featureData$eventMean.z <- tapply(output$z, output$epoch_event, mean)
      featureData$eventSd.z <-tapply(output$z,output$epoch_event,sd.default)
      featureData$eventAbs.z <- tapply(abs(output$dz), output$epoch_event, mean)
      featureData$eventSkew.z <- tapply(output$z,output$epoch_event,skewness)
      featureData$eventKurt.z <- tapply(output$z,output$epoch_event,kurtosis)
      featureData$eventMax.z <- tapply(output$z,output$epoch_event,max)
      featureData$eventMin.z <- tapply(output$z,output$epoch_event,min)
      featureData$eventRange.z <- featureData$eventMax.z - featureData$eventMin.z
      featureData$eventMed.z <- tapply(output$z,output$epoch_event,median)
      featureData$eventMad.z <- tapply(output$z,output$epoch_event,mad)
      featureData$InvCovar.z <- featureData$eventSd.z / featureData$eventMean.z
      featureData$eventLQ.z <- tapply(output$z,output$epoch_event, quantile, 0.25)
      featureData$eventUQ.z <- tapply(output$z,output$epoch_event, quantile, 0.75)
      featureData$event10.z <- tapply(output$z,output$epoch_event, quantile, 0.10)
      featureData$event90.z <- tapply(output$z,output$epoch_event, quantile, 0.90)
      featureData$event90R10.z <- featureData$event90.z - featureData$event10.z
      featureData$eventIQR.z <- featureData$eventUQ.z - featureData$eventLQ.z
      
      ###### obtaining event names in this way preserves the ordering (unique is reordering the variables)
      Events <- names(featureData$eventMad.z)
      nEvents <- length(Events)
      
      Ids <- list(rep.int(SealNames[j], length(Events)))
      
      ###ODBA and VeDBA AUC
      outputs<-NULL
      
      for(i in 1:length(Events)){
        dat<-output[output$epoch_event==Events[i],]
        ODBA.auc   <- MESS::auc(dat$time,   dat$ODBA) 
        VeDBA.auc   <- MESS::auc(dat$time,   dat$VeDBA)  
        new.dat<-cbind(VeDBA.auc,ODBA.auc)
        outputs<-rbind(outputs,new.dat)
      }
      
        
      #Variable Q
      output$ss<-sqrt((output$x)^2+(output$y)^2+(output$z)^2)
      featureData$Qstat <- tapply(output$ss,output$epoch_event,mean)
      
      ##Trend value
      #is the coefficient from the linear regression between an axis (xyz) 
      #and time? Yes
      # not sure if you want an intercept here? probably do, but one is not included yet
      
      featureData$trend.x <- tapply(output$x,output$epoch_event, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      featureData$trend.y <- tapply(output$y,output$epoch_event, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      featureData$trend.z <- tapply(output$z,output$epoch_event, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      #Make new variables for summaries
      #not sure how to summarise these yet...  
      # you nee to calculae a circular variance
      # browser()
      featureData$inclination.cir <- tapply(acos(output$z/output$ss),output$epoch_event, circular::var)
      featureData$azimuth.cir <- tapply(atan(output$y/output$z),output$epoch_event, var, na.rm=T)
    
      
      ###calculating correlations
      featureData$Acf.x <- matrix(0,nrow=nEvents,ncol=1)    
      featureData$Acf.y <- matrix(0,nrow=nEvents,ncol=1)
      featureData$Acf.z <- matrix(0,nrow=nEvents,ncol=1)
      featureData$Corr.xy <- matrix(0,nrow=nEvents,ncol=1)
      featureData$Corr.xz <- matrix(0,nrow=nEvents,ncol=1)
      featureData$Corr.yz <- matrix(0,nrow=nEvents,ncol=1)
      
      ####
      #       getting lots of NA's in here...maybe remove???????
      ###
      
     # for(EE in 1:nEvents){
        #         if(j==4 && k==4 && EE==57) browser()
    #    Idx <- output$epoch_event==Events[EE]
     #   EndIdx <- sum(Idx)
      #  if(EndIdx>1){
       #   featureData$Acf.x[EE] <- cor(output[Idx,"x"][1:(EndIdx-1)], output[Idx,"x"][2:EndIdx])
      #    featureData$Acf.y[EE] <- cor(output[Idx,"y"][1:(EndIdx-1)], output[Idx,"y"][2:EndIdx])
      #    featureData$Acf.z[EE] <- cor(output[Idx,"z"][1:(EndIdx-1)], output[Idx,"z"][2:EndIdx])
      #    featureData$Corr.xy[EE] <- cor(output[Idx,"x"], output[Idx,"y"])
      #    featureData$Corr.xz[EE] <- cor(output[Idx,"x"], output[Idx,"z"])
      #    featureData$Corr.yz[EE] <- cor(output[Idx,"y"], output[Idx,"z"])
      #  }
      #}
      
 
      #       browser()
      featureData$SealName <-  SealNames[j]
      featureData$FileDate <- substr(FileNames[k],1,8)
      #featureData$behaviour<-tapply(output$behav_levels,as.factor(output$epoch_event),Mode)
      featureData$nRows <- tapply(output$epoch_event, output$epoch_event, length)
      
      featureData <- data.frame(featureData)
      featureData <- cbind(featureData,outputs)
      
      #behav_labs<-levels(output$type)
      #featureData$behav_cat<-factor(featureData$behaviour,
       #                             levels=c(1:length(behav_labs)),
        #                            labels=behav_labs)
        
    
      ##add place
       uPlace <- unique(output$place)
      
      if(length(uPlace) > 1){
        Place <- dcast(data = output,
                               formula = epoch_event ~ place, fun.aggregate = length, value.var = "place")[1:length(uPlace)+1]
        labels <- names(Place)
        
        featureData$Place <- labels[max.col(Place)]

      }else{
       featureData$Place <- uPlace
        
     }

      featureData$EventIds <- factor(sapply(Events, function(EE){strsplit(EE, split = "[.]")[[1]][2]}))
      
      #if(any(featureData$Place == "land" && featureData$EventIds == "Foraging")) browser()
      # if(any(featureData$Place == "underwater") && any(featureData$EventIds == "Grooming")) browser()
     
     
     ####keep only behaviours of at least length of chose epoch
        
      featureData<-featureData[featureData$nRows==epochs,]
     
     ###join with other data
      outputData <- rbind(outputData, featureData)
      
    } #end k
    
  } # end j
  
  save(outputData, file=paste0("output/captive_feature_20Hz/outputData_",family,epochs,".RData"))
  #write.csv(outputData,"outputData.csv")
  #file=paste0("C:users/admin/Google Drive/Behaviour accelerometry/Data/Captive/featureData", date(), ".csv"))
  #return(outputData)
}
