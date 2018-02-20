behaviourCodeLoop <- function(PlotBeahviour=FALSE,family){
  
  source("r/captive/behaviourCode.r")
  
  behaviourLoc <- paste0("data/",family,"/behaviour_20Hz/")
  

  ###### run behaviourCode on the raw data to encode the behaviours, this should overwrite any previously encoded files

  #PC
  folderNames <- list.files(behaviourLoc)
  folderNames <- folderNames[!(folderNames %in% c("desktop.ini","Icon\r"))]
  
  ##### loop over the different folders 
  for (i in 1:length(folderNames)){
    rawfiles <- list.files(
      paste0(behaviourLoc, folderNames[i]) ) 
    rawfiles <- rawfiles[!(rawfiles %in% c("desktop.ini","Icon\r"))]
    
    ##### loop over the different files in each folder
    for (k in 1:length(rawfiles)){
      tempFileNameLoc <- paste0(behaviourLoc,
                                folderNames[i],
                                "/",
                                rawfiles[k])
      cat(tempFileNameLoc, "\n")
      
      rawData <- read.csv(tempFileNameLoc,
                          header=TRUE,
                          stringsAsFactors=FALSE)
      
      options(digits.secs = 3)
      a<-nchar(rawData$time[1])
      if(a<=20){
      rawData$date<-as.POSIXct(rawData$time,format="%d/%m/%Y %H:%M:%OS")
      rawData$time<-as.factor(rawData$date)
      }else{
        rawData$date<-as.POSIXct(rawData$time,format="%Y-%m-%d %H:%M:%OS")
        rawData$time<-as.factor(rawData$date)
      }
      
      
      place<-ifelse(rawData$location=="water"&rawData$depth<=1.7,"surface",
      ifelse(rawData$location=="water"&rawData$depth>1.7,"underwater",
             "land"))
      
      rawData$place<-place
      ##### encode the behaviours in the files
      #codedData <- behaviourCode(rawData)
      write.csv(rawData, file=tempFileNameLoc, row.names = FALSE)
      
      
    }
  }
}