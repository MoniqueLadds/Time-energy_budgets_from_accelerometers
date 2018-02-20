behaviourCodeLoop <- function(PlotBeahviour=FALSE){
  
  
  behaviourLoc <- "data/fur seals/behaviour_rawdata/"
  

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
      
      rawData$date<-as.POSIXct(rawData$time,format="%H:%M:%OS")
      rawData$time<-as.factor(rawData$date)
      
      
      ##### encode the behaviours in the files
      codedData <- behaviourCode(rawData)
      write.csv(codedData, file=tempFileNameLoc, row.names = FALSE)
      
      
    }
  }
}