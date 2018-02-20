featureProcessing <- function(epochs = 25,family="sea lions"){
  library(e1071)  
  #install.packages("circular")
  library("circular", quietly=TRUE)
  #install.packages("MESS")
  library(MESS)
  library(reshape2)
  #install.packages("caTools")
  require(caTools)
  library(pracma)
  source("r/functions.R")
  
  fileLoc <- paste0("data/",family,"/behaviour_1Hzclean/")
 #    setwd("C:/Users/admin/Google Drive/Behaviour accelerometry")
     
    SealNames <- list.files(fileLoc)
  #for PC
  SealNames <- SealNames[!grepl("desktop.ini", SealNames)]
  SealNames <- SealNames[!grepl("Icon\r", SealNames)]
  
  # ##### test j, k
      #j=12
      #k=2
  #     browser()
  outputData <- NULL
  for(j in 1:length(SealNames)){
    FileNames <- list.files(paste(fileLoc, SealNames[j], sep="/"))
    FileNames <- FileNames[!grepl("desktop.ini", FileNames)]
    FileNames <- FileNames[!grepl("Thumbs.db", FileNames)]
    nFileNames <- length(FileNames)
    
    cat("j=",j,"\n")
    for(k in 1:nFileNames){
      cat("k=",k,"\n")
      
      #       if(j==8 && k==1){
      #         browser()
      #       }
      
      ##### read in the data
      cat(paste(fileLoc,
                SealNames[j],
                FileNames[k],
                sep="/"),
          "\n")
      
      
      load(paste(fileLoc, SealNames[j], FileNames[k],  sep="/"))
      
      output$place<-ifelse(output$place=="land","land",ifelse(output$depth>1.7,"underwater","surface"))
      
      ####################################################
      ####################################################
       # browser()
      op <- options(digits.secs = 1)
      # options(op) #using this seems to return to the default behaviour of no decimal seconds
      #output$time <- as.POSIXct(output$time, format="%Y-%m-%d %H:%M:%S")
      output$TrialDate <- substr(FileNames[k],start = 1, stop = 8)
      #reduce number of columns      
      output <- output[,c("date","TrialDate","type","x","y","z","behaviour","place","depth")]
      output$sealName <- SealNames[j]
      
      ##remove duplicated times
      output<-output[!duplicated(output[,c(1:6)]),]
      
      ###remove behaviours that occur for less than 2 seconds    
      #output<-output[unsplit(table(output$type_event), output$type_event) >= 3, ]
     
      
      ###choose epoch_event length by which to summarise features
      epoch<-rep(1:(nrow(output)/epochs), each=epochs)
      output<-output[1:length(epoch),]
      output$epoch<-epoch    
      
      output$epoch_event<-paste0(output$epoch,".",output$type)
      #output$type_event<-(as.factor(output$type_event))
      output$type<-(as.factor(output$type))
      output$behav_levels<-as.numeric(output$type)
      
      #filter x, y, z for noise
      output$x<-hampel(output$x, k=3)$y
      
      output$y<-hampel(output$y, k=3)$y
      
      output$z<-hampel(output$z, k=3)$y
      
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
      output$pitch<-tan(-output$x/(sqrt(output$y+output$z)))*180/pi
      output$roll<-atan2(output$y,output$z)*180/pi
      output$inclination<-tan(sqrt(output$x+output$y)/output$z)*180/pi
      
      movvar<-abs(diff(output$x,lag=1))+abs(diff(output$y,lag=1))+abs(diff(output$z,lag=1))
      output<-output[-1,]
      output$movvar<-movvar
      
      #### data storage
      featureData <- NULL
      
      ##### summarise some shit
      featureData$pdbax<-tapply(output$grav.x,output$epoch_event,mean)
      featureData$pdbay<-tapply(output$grav.y,output$epoch_event,mean)
      featureData$pdbaz<-tapply(output$grav.z,output$epoch_event,mean)
      featureData$dx<-tapply(output$DynODBA.x,output$epoch_event,mean)
      featureData$dy<-tapply(output$DynODBA.y,output$epoch_event,mean)
      featureData$dz<-tapply(output$DynODBA.z,output$epoch_event,mean)
     
      ###calculate ODBA
      featureData$ODBA.mean<-tapply(output$ODBA,output$epoch_event,mean)
      ###calculate VeDBA      
      featureData$VeDBA.mean<-tapply(output$VeDBA,output$epoch_event,mean)
      
          
      ##make variables for summary file###
      #All x summaries
      featureData$eventMean.x <- tapply(output$x, output$epoch_event, mean)
      featureData$eventSd.x <-tapply(output$x,output$epoch_event,sd.default)
      featureData$eventAbs.x <- tapply(abs(output$DynODBA.x), output$epoch_event, mean)
      featureData$eventSkew.x <- tapply(output$x,output$epoch_event,skewness)
      featureData$eventKurt.x <- tapply(output$x,output$epoch_event,kurtosis)
      featureData$eventMax.x <- tapply(output$x,output$epoch_event,max)
      featureData$eventMin.x <- tapply(output$x,output$epoch_event,min)
      featureData$eventMed.x <- tapply(output$x,output$epoch_event,median)
      featureData$eventMad.x <- tapply(output$x,output$epoch_event,mad)
      featureData$InvCovar.x <- featureData$eventSd.x / featureData$eventMean.x
      
      
      #All Y summaries
      featureData$eventMean.y <- tapply(output$y, output$epoch_event, mean)
      featureData$eventSd.y <-tapply(output$y,output$epoch_event,sd.default)
      featureData$eventAbs.y <- tapply(abs(output$DynODBA.y), output$epoch_event, mean)
      featureData$eventSkew.y <- tapply(output$y,output$epoch_event,skewness)
      featureData$eventKurt.y <- tapply(output$y,output$epoch_event,kurtosis)
      featureData$eventMax.y <- tapply(output$y,output$epoch_event,max)
      featureData$eventMin.y <- tapply(output$y,output$epoch_event,min)
      featureData$eventMed.y <- tapply(output$y,output$epoch_event,median)
      featureData$eventMad.y <- tapply(output$y,output$epoch_event,mad)
      featureData$InvCovar.y <- featureData$eventSd.y / featureData$eventMean.y
      
      
      
      #All Z summaries
      featureData$eventMean.z <- tapply(output$z, output$epoch_event, mean)
      featureData$eventSd.z <-tapply(output$z,output$epoch_event,sd.default)
      featureData$eventAbs.z <- tapply(abs(output$DynODBA.z), output$epoch_event, mean)
      featureData$eventSkew.z <- tapply(output$z,output$epoch_event,skewness)
      featureData$eventKurt.z <- tapply(output$z,output$epoch_event,kurtosis)
      featureData$eventMax.z <- tapply(output$z,output$epoch_event,max)
      featureData$eventMin.z <- tapply(output$z,output$epoch_event,min)
      featureData$eventMed.z <- tapply(output$z,output$epoch_event,median)
      featureData$eventMad.z <- tapply(output$z,output$epoch_event,mad)
      featureData$InvCovar.z <- featureData$eventSd.z / featureData$eventMean.z
      
      ###### obtaining event names in this way preserves the ordering (unique is reordering the variables)
      Events <- names(featureData$eventMad.z)
      nEvents <- length(Events)
      
      Ids <- list(rep.int(SealNames[j], length(Events)))
      
      ###ODBA and VeDBA AUC
      outputs<-NULL
      
      for(i in 1:length(Events)){
        dat<-output[output$epoch_event==Events[i],]
        ODBA.auc   <- auc(dat$date,   dat$ODBA) 
        VeDBA.auc   <- auc(dat$date,   dat$VeDBA)  
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
      #featureData$Acf.x <- matrix(0,nrow=nEvents,ncol=1)    
      #featureData$Acf.y <- matrix(0,nrow=nEvents,ncol=1)
      #featureData$Acf.z <- matrix(0,nrow=nEvents,ncol=1)
      #featureData$Corr.xy <- matrix(0,nrow=nEvents,ncol=1)
      #featureData$Corr.xz <- matrix(0,nrow=nEvents,ncol=1)
      #featureData$Corr.yz <- matrix(0,nrow=nEvents,ncol=1)
      
      ####
      #       getting lots of NA's in here...maybe remove???????
      ###
      
      #for(EE in 1:nEvents){
        #         if(j==4 && k==4 && EE==57) browser()
       # Idx <- output$epoch_event==Events[EE]
      #  EndIdx <- sum(Idx)
       # if(EndIdx>1){
        #  featureData$Acf.x[EE] <- cor(output[Idx,"x"][1:(EndIdx-1)], output[Idx,"x"][2:EndIdx])
        #  featureData$Acf.y[EE] <- cor(output[Idx,"y"][1:(EndIdx-1)], output[Idx,"y"][2:EndIdx])
        #  featureData$Acf.z[EE] <- cor(output[Idx,"z"][1:(EndIdx-1)], output[Idx,"z"][2:EndIdx])
        #  featureData$Corr.xy[EE] <- cor(output[Idx,"x"], output[Idx,"y"])
        #  featureData$Corr.xz[EE] <- cor(output[Idx,"x"], output[Idx,"z"])
        #  featureData$Corr.yz[EE] <- cor(output[Idx,"y"], output[Idx,"z"])
        #}
    #  }
      
 
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
     
     
     ####keep only behaviours of at least length of chosen epoch
        
      featureData<-featureData[featureData$nRows==epochs,]
     
     ###join with other data
      outputData <- rbind(outputData, featureData)
      
    } #end k
    
  } # end j
  
  save(outputData, file=paste0("output/captive_feature_1Hz/outputData_",family,epochs,".RData"))
  #write.csv(outputData,"outputData.csv")
  #file=paste0("C:users/admin/Google Drive/Behaviour accelerometry/Data/Captive/featureData", date(), ".csv"))
  #return(outputData)
}
