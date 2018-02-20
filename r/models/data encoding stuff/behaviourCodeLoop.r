behaviourCodeLoop <- function(PlotBeahviour = FALSE,
                              nBehaviours = 4,
                              rFileDir = "~/Documents/r_files/Seals",
                              behaviourLoc = "~/Documents/r_files/Seals/behaviourFinal",
                              saveBehaviourLocFT = "~/Documents/r_files/Seals/behaviour3FT",
                              FT = FALSE,
                              freq = 13){
  
  library(e1071)
  
#   if(.Platform$OS.type=="unix"){
#     #MAC
    source(file.path(rFileDir, "behaviourCode.r"))
#     behaviourLoc <- "~/Google Drive/Behaviour accelerometry/data/Captive/behaviour raw/"  
#   }else{
#     #PC
#     source("c:/users/admin/Google Drive/Behaviour accelerometry/R/behaviourCode.R")
#     behaviourLoc <- "C:/users/admin/Google Drive/Behaviour accelerometry/data/Captive/behaviour raw/"
#   }
  ###### run behaviourCode on the raw data to encode the behaviours, this should overwrite any previously encoded files
  #MAC
  #PC
  folderNames <- list.files(behaviourLoc)
  folderNames <- folderNames[!(folderNames %in% c("desktop.ini","Icon\r"))]
  
  if(nBehaviours == 7){
  behaviourCodeTable <- read.csv(file="~/Documents/r_files/Seals/behaviourCodes7Behaviours.csv",
                                 stringsAsFactors=FALSE)
  saveBehaviourLoc = "~/Documents/r_files/Seals/behaviourFinalSave"
  }
  if(nBehaviours == 4){
    behaviourCodeTable <- read.csv(file="~/Documents/r_files/Seals/behaviourCodes2.csv",
                                   stringsAsFactors=FALSE)    
    saveBehaviourLoc = "~/Documents/r_files/Seals/behaviour4FinalSave"
  }
  if(FT){
    outputData <- NULL
  }
  
  ##### loop over the different folders 
  for (i in 1:length(folderNames)){
    rawfiles <- list.files(
      file.path(behaviourLoc, folderNames[i]) ) 
    rawfiles <- rawfiles[!(rawfiles %in% c("desktop.ini","Icon\r"))]
    
    ##### loop over the different files in each folder
    for (k in 1:length(rawfiles)){
      # browser()
      tempFileNameLoc <- file.path(behaviourLoc,
                                folderNames[i],
                                rawfiles[k])
      cat(tempFileNameLoc, "\n")
      
      rawData <- read.csv(tempFileNameLoc,
                          header=TRUE,
                          stringsAsFactors=FALSE)
      
#       rawData <- read.csv("~/Documents/r_files/Seals/behaviourFinal/ronnie/12-09-14.csv",
#                           header=TRUE,
#                           stringsAsFactors=FALSE)
      
      ##### encode the behaviours in the files
      if(FT){
      codedDataFT <- behaviourCode(inputData = rawData,
                                 freq = freq,
                                 behaviourCodeTable = behaviourCodeTable,
                                 FT = FT)
      }
      # browser()
      codedData <- behaviourCode(inputData = rawData,
                                 freq = freq,
                                 behaviourCodeTable = behaviourCodeTable,
                                 FT = FALSE)
      
      # browser()
      if(FT){
      featData <- featureProcessingSubFun(codedData, folderNames[i])
      
      codedData <- dplyr::left_join(x = codedDataFT, y = featData, by = "type_event2")
      

        write.csv(codedData,
                file = file.path(saveBehaviourLocFT,
                              folderNames[i],
                              rawfiles[k]),
                row.names = FALSE)
        codedData$EventIds2 <- sapply(codedData$type_event2, function(EE){strsplit(EE, split = "[.]")[[1]][2]})
        
        codedData$sealName <- folderNames[i]
        outputData <- rbind(outputData, codedData)
      }else{
        write.csv(codedData,
                  file = file.path(saveBehaviourLoc,
                                   folderNames[i],
                                   rawfiles[k]),
                  row.names = FALSE)
      }
      
    }
  }
  if(FT){
    return(outputData)
  }
}