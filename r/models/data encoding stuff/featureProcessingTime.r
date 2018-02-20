featureProcessing <- function(nBehaviours = NULL, targetCol = "type_event75"){
  
  if(is.null(nBehaviours))
    stop("need to give the number of behaviours, 4 or 7\n")
  
  library(e1071)  
  library("circular", quietly=TRUE)
  library("reshape2")
  library(MESS)
  
  Rcpp::sourceCpp('~/Documents/r_files/Seals/groupMode.cpp')
  #setwd("/Users/aptperson/Google Drive/Behaviour accelerometry/")
if(.Platform$OS.type=="unix"){
    #MAC
    #     source("~/Google Drive/Behaviour accelerometry/R/behaviourCode.R")
    # fileLoc <- "~/Google Drive/Behaviour accelerometry/data/Captive/behaviour raw/"  
    
    if(nBehaviours == 7){
      fileLoc = "~/Documents/r_files/Seals/behaviourFinalSave"
    }
    if(nBehaviours == 4){
      fileLoc = "~/Documents/r_files/Seals/behaviour4FinalSave"
    }    
  }else{
    #PC
    #     source("c:/users/admin/Google Drive/Behaviour accelerometry/R/behaviourCode.R")
    fileLoc <- "C:/users/admin/Google Drive/Behaviour accelerometry/Data/Captive/behaviour raw"
    
  }
  #   fileLoc <- "C:/users/admin/Google Drive/Behaviour accelerometry/Data/Captive/behaviour raw"
  
  SealNames <- list.files(fileLoc)
  #for PC
  SealNames <- SealNames[!grepl("desktop.ini", SealNames)]
  SealNames <- SealNames[!grepl("Icon\r", SealNames)]
  
  

  
  # ##### test j, k
  #     j=9
  #     k=3
  #     browser()
  outputData <- NULL
  for(j in 1:length(SealNames)){
    FileNames <- list.files(paste(fileLoc, SealNames[j], sep="/"))
    FileNames <- FileNames[!grepl("desktop.ini", FileNames)]
    FileNames <- FileNames[!grepl("Icon\r", FileNames)]
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
      inputData <- read.csv(paste(fileLoc,
                                  SealNames[j],
                                  FileNames[k],
                                  sep="/"),
                            header=TRUE,
                            stringsAsFactors=FALSE)
      
      ##### if discrete behaviours, drop end rows of incomplete behaviours
      switch(targetCol,
             "type_event3" = {
               inputData <- inputData[inputData$value3 !=0,]
             },
             "type_event5" = {
               inputData <- inputData[inputData$value5 !=0,]
             },
             "type_event7" = {
               inputData <- inputData[inputData$value7 !=0,]
             },
             "type_event9" = {
               inputData <- inputData[inputData$value9 !=0,]
             },
             "type_event11" = {
               inputData <- inputData[inputData$value11 !=0,]
             },
             "type_event13" = {
               inputData <- inputData[inputData$value13 !=0,]
             },
             "type_event25" = {
               inputData <- inputData[inputData$value25 !=0,]
             },
             "type_event75" = {
               inputData <- inputData[inputData$value75 !=0,]
             })
      
      #reduce number of columns      
      inputData <- inputData[,c(targetCol, "place","x","y","z")]
      ##### assign the target col
      inputData$type_event2 <- inputData[, targetCol]
      inputData[, targetCol] <- NULL
      inputData$sealName <- SealNames[2]
      # browser()
      
      ####apply running mean#####
      ma <- function(x,n=25){stats::filter(x,rep(1/n,n), sides=2)}
      inputData$xma <- ma(inputData$x)
      inputData$yma <- ma(inputData$y)
      inputData$zma <- ma(inputData$z)
      inputData$dx <- inputData$x-inputData$xma 
      inputData$dy <- inputData$y-inputData$yma 
      inputData$dz <- inputData$z-inputData$zma 
      
      ##### remove incomplete data points
      inputData <- inputData[complete.cases(inputData), ]
      
      ###Calculate ODBA
      inputData$ODBA <- (abs(inputData$dx) + abs(inputData$dy) + abs(inputData$dz))
      
      ###Calculate VeDBA
      inputData$VeDBA <- sqrt(inputData$dx^2 + inputData$dy^2 + inputData$dz^2)
      
      ##### data storage
      featureData <- NULL
      
      # browser()
      ##### summarise some shit
      featureData$pdbax<-tapply(inputData$xma,inputData$type_event2,mean)
      featureData$pdbay<-tapply(inputData$yma,inputData$type_event2,mean)
      featureData$pdbaz<-tapply(inputData$zma,inputData$type_event2,mean)
      featureData$dx<-tapply(inputData$dx,inputData$type_event2,mean)
      featureData$dy<-tapply(inputData$dy,inputData$type_event2,mean)
      featureData$dz<-tapply(inputData$dz,inputData$type_event2,mean)
      
      featureData$pdbaxMin<-tapply(inputData$xma,inputData$type_event2,min)
      featureData$pdbayMin <-tapply(inputData$yma,inputData$type_event2,min)
      featureData$pdbazMin <-tapply(inputData$zma,inputData$type_event2,min)
      featureData$dxMin <-tapply(inputData$dx,inputData$type_event2,min)
      featureData$dyMin <-tapply(inputData$dy,inputData$type_event2,min)
      featureData$dzMin <-tapply(inputData$dz,inputData$type_event2,min)
      
      featureData$pdbaxMax <-tapply(inputData$xma,inputData$type_event2,max)
      featureData$pdbayMax <-tapply(inputData$yma,inputData$type_event2,max)
      featureData$pdbazMax <-tapply(inputData$zma,inputData$type_event2,max)
      featureData$dxMax <-tapply(inputData$dx,inputData$type_event2,max)
      featureData$dyMax <-tapply(inputData$dy,inputData$type_event2,max)
      featureData$dzMax <-tapply(inputData$dz,inputData$type_event2,max)
      
      featureData$pdbaxLQ<-tapply(inputData$xma,inputData$type_event2, quantile, 0.25)
      featureData$pdbayLQ <-tapply(inputData$yma,inputData$type_event2, quantile, 0.25)
      featureData$pdbazLQ <-tapply(inputData$zma,inputData$type_event2, quantile, 0.25)
      featureData$dxLQ <-tapply(inputData$dx,inputData$type_event2,quantile, 0.25)
      featureData$dyLQ <-tapply(inputData$dy,inputData$type_event2,quantile, 0.25)
      featureData$dzLQ <-tapply(inputData$dz,inputData$type_event2,quantile, 0.25)
      
      featureData$pdbaxUQ<-tapply(inputData$xma,inputData$type_event2, quantile, 0.75)
      featureData$pdbayUQ <-tapply(inputData$yma,inputData$type_event2, quantile, 0.75)
      featureData$pdbazUQ <-tapply(inputData$zma,inputData$type_event2, quantile, 0.75)
      featureData$dxUQ <-tapply(inputData$dx,inputData$type_event2,quantile, 0.75)
      featureData$dyUQ <-tapply(inputData$dy,inputData$type_event2,quantile, 0.75)
      featureData$dzUQ <-tapply(inputData$dz,inputData$type_event2,quantile, 0.75)
      
      featureData$pdbax10<-tapply(inputData$xma,inputData$type_event2, quantile, 0.10)
      featureData$pdbay10 <-tapply(inputData$yma,inputData$type_event2, quantile, 0.10)
      featureData$pdbaz10 <-tapply(inputData$zma,inputData$type_event2, quantile, 0.10)
      featureData$pdbax90<-tapply(inputData$xma,inputData$type_event2, quantile, 0.90)
      featureData$pdbay90 <-tapply(inputData$yma,inputData$type_event2, quantile, 0.90)
      featureData$pdbaz90 <-tapply(inputData$zma,inputData$type_event2, quantile, 0.90)
      featureData$dx10 <-tapply(inputData$dx,inputData$type_event2,quantile, 0.10)
      featureData$dy10 <-tapply(inputData$dy,inputData$type_event2,quantile, 0.10)
      featureData$dz10 <-tapply(inputData$dz,inputData$type_event2,quantile, 0.10)
      featureData$dx90 <-tapply(inputData$dx,inputData$type_event2,quantile, 0.90)
      featureData$dy90 <-tapply(inputData$dy,inputData$type_event2,quantile, 0.90)
      featureData$dz90 <-tapply(inputData$dz,inputData$type_event2,quantile, 0.90)
      
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
      featureData$odba <- tapply(inputData$ODBA,
                                 inputData$type_event2,
                                 mean)
      featureData$odbaMAX <- tapply(inputData$ODBA,
                                 inputData$type_event2,
                                 max)
      featureData$odbaMin <- tapply(inputData$ODBA,
                                 inputData$type_event2,
                                 min)
      featureData$odbaUQ <- tapply(inputData$ODBA,
                                 inputData$type_event2,
                                 quantile, 0.75)
      featureData$odbaLQ <- tapply(inputData$ODBA,
                                 inputData$type_event2,
                                 quantile, 0.25)
      featureData$odbaIQR <- featureData$odbaUQ - featureData$odbaLQ
      featureData$odbaMedian <- tapply(inputData$ODBA,
                                 inputData$type_event2,
                                 median)
      featureData$odbaSD <- tapply(inputData$ODBA,
                                 inputData$type_event2,
                                 sd)
      featureData$odba10 <- tapply(inputData$ODBA,
                                   inputData$type_event2,
                                   quantile, 0.10)
      featureData$odba90 <- tapply(inputData$ODBA,
                                   inputData$type_event2,
                                   quantile, 0.90)
      featureData$odba90R10 <- featureData$odba90 - featureData$odba10
      featureData$odbaRange <- featureData$odbaMAX - featureData$odbaMin
      
      ###calculate VeDBA      
      featureData$VeDBA.mean<-tapply(inputData$VeDBA,
                                     inputData$type_event2,
                                     mean)
      featureData$VeDBAMAX <- tapply(inputData$VeDBA,
                                    inputData$type_event2,
                                    max)
      featureData$VeDBAMin <- tapply(inputData$VeDBA,
                                    inputData$type_event2,
                                    min)
      featureData$VeDBAUQ <- tapply(inputData$VeDBA,
                                   inputData$type_event2,
                                   quantile, 0.75)
      featureData$VeDBALQ <- tapply(inputData$VeDBA,
                                   inputData$type_event2,
                                   quantile, 0.25)
      featureData$VeDBAIQR <- featureData$VeDBAUQ - featureData$VeDBALQ
      featureData$VeDBAMedian <- tapply(inputData$VeDBA,
                                       inputData$type_event2,
                                       median)
      featureData$VeDBASD <- tapply(inputData$VeDBA,
                                   inputData$type_event2,
                                   sd)
      featureData$VeDBA10 <- tapply(inputData$VeDBA,
                                   inputData$type_event2,
                                   quantile, 0.10)
      featureData$VeDBA90 <- tapply(inputData$VeDBA,
                                   inputData$type_event2,
                                   quantile, 0.90)
      featureData$VeDBA90R10 <- featureData$VeDBA90 - featureData$VeDBA10
      featureData$VeDBARange <- featureData$VeDBAMAX - featureData$VeDBAMin
      
      ##make variables for summary file###
      #All x summaries
      featureData$eventMean.x <- tapply(inputData$x, inputData$type_event2, mean)
      featureData$eventSd.x <-tapply(inputData$x,inputData$type_event2,sd.default)
      featureData$eventAbs.x <- tapply(abs(inputData$dx), inputData$type_event2, mean)
      featureData$eventSkew.x <- tapply(inputData$x,inputData$type_event2,skewness)
      featureData$eventKurt.x <- tapply(inputData$x,inputData$type_event2,kurtosis)
      featureData$eventMax.x <- tapply(inputData$x,inputData$type_event2,max)
      featureData$eventMin.x <- tapply(inputData$x,inputData$type_event2,min)
      featureData$eventRange.x <- featureData$eventMax.x - featureData$eventMin.x 
      featureData$eventMed.x <- tapply(inputData$x,inputData$type_event2,median)
      featureData$eventMad.x <- tapply(inputData$x,inputData$type_event2,mad)
      featureData$InvCovar.x <- featureData$eventSd.x / featureData$eventMean.x
      featureData$eventLQ.x <- tapply(inputData$x,inputData$type_event2, quantile, 0.25)
      featureData$eventUQ.x <- tapply(inputData$x,inputData$type_event2, quantile, 0.75)
      featureData$event10.x <- tapply(inputData$x,inputData$type_event2, quantile, 0.10)
      featureData$event90.x <- tapply(inputData$x,inputData$type_event2, quantile, 0.90)
      featureData$event90R10.x <- featureData$event90.x - featureData$event10.x
      featureData$eventIQR.x <- featureData$eventUQ.x - featureData$eventLQ.x

            #All Y summaries
      featureData$eventMean.y <- tapply(inputData$y, inputData$type_event2, mean)
      featureData$eventSd.y <-tapply(inputData$y,inputData$type_event2,sd.default)
      featureData$eventAbs.y <- tapply(abs(inputData$dy), inputData$type_event2, mean)
      featureData$eventSkew.y <- tapply(inputData$y,inputData$type_event2,skewness)
      featureData$eventKurt.y <- tapply(inputData$y,inputData$type_event2,kurtosis)
      featureData$eventMax.y <- tapply(inputData$y,inputData$type_event2,max)
      featureData$eventMin.y <- tapply(inputData$y,inputData$type_event2,min)
      featureData$eventRange.y <- featureData$eventMax.y - featureData$eventMin.y 
      featureData$eventMed.y <- tapply(inputData$y,inputData$type_event2,median)
      featureData$eventMad.y <- tapply(inputData$y,inputData$type_event2,mad)
      featureData$InvCovar.y <- featureData$eventSd.y / featureData$eventMean.y
      featureData$eventLQ.y <- tapply(inputData$y,inputData$type_event2, quantile, 0.25)
      featureData$eventUQ.y <- tapply(inputData$y,inputData$type_event2, quantile, 0.75)
      featureData$event10.y <- tapply(inputData$y,inputData$type_event2, quantile, 0.10)
      featureData$event90.y <- tapply(inputData$y,inputData$type_event2, quantile, 0.90)
      featureData$event90R10.y <- featureData$event90.y - featureData$event10.y
      featureData$eventIQR.y <- featureData$eventUQ.y - featureData$eventLQ.y

            #All Z summaries
      featureData$eventMean.z <- tapply(inputData$z, inputData$type_event2, mean)
      featureData$eventSd.z <-tapply(inputData$z,inputData$type_event2,sd.default)
      featureData$eventAbs.z <- tapply(abs(inputData$dz), inputData$type_event2, mean)
      featureData$eventSkew.z <- tapply(inputData$z,inputData$type_event2,skewness)
      featureData$eventKurt.z <- tapply(inputData$z,inputData$type_event2,kurtosis)
      featureData$eventMax.z <- tapply(inputData$z,inputData$type_event2,max)
      featureData$eventMin.z <- tapply(inputData$z,inputData$type_event2,min)
      featureData$eventRange.z <- featureData$eventMax.z - featureData$eventMin.z
      featureData$eventMed.z <- tapply(inputData$z,inputData$type_event2,median)
      featureData$eventMad.z <- tapply(inputData$z,inputData$type_event2,mad)
      featureData$InvCovar.z <- featureData$eventSd.z / featureData$eventMean.z
      featureData$eventLQ.z <- tapply(inputData$z,inputData$type_event2, quantile, 0.25)
      featureData$eventUQ.z <- tapply(inputData$z,inputData$type_event2, quantile, 0.75)
      featureData$event10.z <- tapply(inputData$z,inputData$type_event2, quantile, 0.10)
      featureData$event90.z <- tapply(inputData$z,inputData$type_event2, quantile, 0.90)
      featureData$event90R10.z <- featureData$event90.z - featureData$event10.z
      featureData$eventIQR.z <- featureData$eventUQ.z - featureData$eventLQ.z
      
      ###### obtaining event names in this way preserves the ordering (unique is reordering the variables)
      Events <- names(featureData$eventMad.z)
      nEvents <- length(Events)
      
      Ids <- list(rep.int(SealNames[1], length(Events)))
      
      
      #Variable Q
      inputData$ss <- sqrt((inputData$x)^2+(inputData$y)^2+(inputData$z)^2)
      featureData$Qstat <- tapply(inputData$ss,inputData$type_event2,mean)
      
      ##Trend value
      #is the coefficient from the linear regression between an azis (zyz) 
      #and time? Yes
      # not sure if you want an intercept here? probably do, but one is not included yet
      
      featureData$trend.x <- tapply(inputData$x,inputData$type_event2, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      featureData$trend.y <- tapply(inputData$y,inputData$type_event2, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      featureData$trend.z <- tapply(inputData$z,inputData$type_event2, FUN=function(yy){
        lm.fit(x = as.matrix(1:length(yy)), y = yy)$coefficients[1]})
      
      #Make new variables for summaries
      #not sure how to summarise these yet...  
      # you nee to calculate a circular variance
      featureData$inclination.cir <- tapply(acos(inputData$z/inputData$ss),inputData$type_event2, var)
      featureData$azimuth.cir <- tapply(atan(inputData$y/inputData$z),inputData$type_event2, var, na.rm=T)
      
      featureData$Acf.x <- matrix(0,nrow=nEvents,ncol=1)    
      featureData$Acf.y <- matrix(0,nrow=nEvents,ncol=1)
      featureData$Acf.z <- matrix(0,nrow=nEvents,ncol=1)
      featureData$Corr.xy <- matrix(0,nrow=nEvents,ncol=1)
      featureData$Corr.xz <- matrix(0,nrow=nEvents,ncol=1)
      featureData$Corr.yz <- matrix(0,nrow=nEvents,ncol=1)
      
      for(EE in 1:nEvents){
        Idx <- inputData$type_event2==Events[EE]
        EndIdx <- sum(Idx)
        if(EndIdx>1){
          featureData$Acf.x[EE] <- cor(inputData[Idx,"x"][1:(EndIdx-1)], inputData[Idx,"x"][2:EndIdx])
          featureData$Acf.y[EE] <- cor(inputData[Idx,"y"][1:(EndIdx-1)], inputData[Idx,"y"][2:EndIdx])
          featureData$Acf.z[EE] <- cor(inputData[Idx,"z"][1:(EndIdx-1)], inputData[Idx,"z"][2:EndIdx])
          featureData$Corr.xy[EE] <- cor(inputData[Idx,"x"], inputData[Idx,"y"])
          featureData$Corr.xz[EE] <- cor(inputData[Idx,"x"], inputData[Idx,"z"])
          featureData$Corr.yz[EE] <- cor(inputData[Idx,"y"], inputData[Idx,"z"])
        }
      }
      featureData$SealName <-  SealNames[j]
      featureData$FileDate <- FileNames[k]
      featureData$nRows <- tapply(inputData$type_event2, inputData$type_event2, length)
      
      featureData <- data.frame(featureData)
      
      uPlace <- unique(inputData$place)
      
      if(length(uPlace) > 1){
      # browser()
        Place <- dcast(data = inputData,
                       formula = type_event2 ~ place, fun.aggregate = length, value.var = "place")[1:length(uPlace)+1]
        labels <- names(Place)
        featureData$Place <- labels[max.col(Place)]
      
      }else{
        featureData$Place <- uPlace
        
      }
      featureData$EventIds <- sapply(Events, function(EE){strsplit(EE, split = "[.]")[[1]][2]})
      outputData <- rbind(outputData, featureData)
      # print(table(outputData$EventIds))
      # browser()
    } #end k
    
  } # end j
  
  ##### change character columns to factor
  outputData[sapply(outputData, is.character)] <- lapply(outputData[sapply(outputData, is.character)], 
                                         as.factor)
  
  
  write.csv(outputData, paste0("outputData4Behaviours",targetCol, ".csv"), row.names = FALSE)
  #file=paste0("C:users/admin/Google Drive/Behaviour accelerometry/Data/Captive/featureData", date(), ".csv"))
  return(outputData)
}
