accRun <- function(featureData,
                   testData,
                   Model = "RF",
                   codeTest = TRUE,
                   Parallel = TRUE,
                   K = 10,
                   SAVE = TRUE,
                   Dummies = FALSE,
                   Cores = 4,
                   printSummary = FALSE,
                   classMax = 500,
                   Kernel = "linear"){
  library(caret)
  library(randomForest)
  library(glmnet)
  library(e1071)
  require(devtools)
  ###### remove NA's from the data
  # browser()
  featureData <- featureData[complete.cases(featureData),]
  
  
  ##### feature imputation for missing values
  # mean imputation, nope no missing values! yay!
  
  if(printSummary){
    cat("##### raw data summary:\n")
    print(table(featureData$EventIds))
  }
  
  
  ##### remove class other  
  featureData <- featureData[featureData$EventIds!="Other",]
  # browser()
  featureData <- featureData[!is.na(featureData$EventIds),]
  featureData <- featureData[featureData$EventIds != "NA",]
  
  ##### remove low length behaviours
  featureData <- featureData[featureData$nRows > 5,]
  
  if(Dummies){
    ##### add in the seal specific features
    sealCharacteristic <- read.csv("I:/Juvenile fur seal energetics/animal_details2.csv",
                                   stringsAsFactors=FALSE)
    
    featureData <- merge(featureData, sealCharacteristic[,1:5], by = "SealName")
    
    featureData$Place <- as.numeric(as.factor(featureData$Place))           
    featureData$Gender <- as.numeric(as.factor(featureData$Gender))
    featureData$species <- as.numeric(as.factor(featureData$species))
    #featureData$harsness <- factor(featureData$harsness)  
  }
  else{
    # browser()
    PlaceVector <- as.factor(featureData$Place)
    featureData$Place <- as.numeric(PlaceVector)
    testData$Place <- as.numeric(factor(x = testData$Place, levels = levels(PlaceVector)))
    # cat("you need to check if the place labels have been converted to the same integers on line 56\n")
    # stop("did you check this?")
    # Adam, yes I checked this
  }
  
  ##### Down sample the large classes
  set.seed(123, "L'Ecuyer")
  featureData$EventIds <- as.character(featureData$EventIds)
  uEventIds <- unique(featureData$EventIds)
  if(codeTest){
    classMax <- 200
    K <- 2
  }else{
    classMax <- classMax
  }
  
  sampledData <- NULL
  for(i in 1:length(uEventIds)){
    
    tempData <- featureData[featureData$EventIds==uEventIds[i],]
    nr <- nrow(tempData)
    
    if(nr>classMax){
      sampleIdx <- sample.int(n = nr, size = classMax)
      tempData <- tempData[sampleIdx,]
    }
    sampledData <- rbind(sampledData, tempData)
  }
  
  if(printSummary){
    cat("##### full training and testing data summary:\n")
    print(table(sampledData$EventIds))
  }
  
  #if(codeTest){
    ###### use this for testing for fast model training
  # trainTestIdx <- sampledData
  #}else{
    ##### full data training and testing
    ##use all of the captive data for training
  # trainTestIdx <- trainData
  #}
  #trainDataSplit=trainTestIdx
  
  #if(codeTest){
    ###### use this for testing for fast model training
  #  testDataIdx <- createDataPartition(y=testData$EventIds, times=1, p = 0.2)
  #  testDataIdx <- testData[testDataIdx$Resample1,]
  #}else{
    ##### full data training and testing
    ##use all of the captive data for training
   # testDataIdx <- testData
  #}
  testDataSplit=testData
  
  ##### remove the indetifier variables
  trainDataSplit <- sampledData[, !(names(sampledData) %in% c("FileDate", "SealName", "nRows", 
                                                         "Acf.x", "Acf.y", "Acf.z", "Corr.xy", "Corr.yz", "Corr.xz"))]
  
  testDataSplit <- testDataSplit[,!(names(testDataSplit) %in%  c("SealName", "Acf.x", "Acf.y", "Acf.z", "Corr.xy", 
                                       "Corr.yz", "Corr.xz","time_stamp"))]
  
  #testDataSplit<-data.frame(lapply(testDataSplit,as.numeric))
  
  
  if(printSummary){
    cat("##### training split data summary:\n")
    print(table(trainDataSplit$EventIds))
    cat("##### testing split data summary:\n")
    print(nrow(testData))
  }
  
  ##### creaete a function to evaluate the final fittness
  fittnessFun <- function(obs,pred){
    ACC <- sum(obs==pred)/length(pred)
  }
  
  switch(Model,
         "XGB" = {
           #source("parallelCVxgb.r")
           if(codeTest){
             K = K
             paramList = expand.grid(eta = 10 ^ - seq(from = 1, to = 1, by = 1),
                                     max.depth = 5,
                                     nrounds = 100,
                                     subsample = c(0.5),
                                     colsample_bytree = 1,
                                     # lambda = seq(from = 0.5, to = 1, by = 0.5),
                                     # alpha = seq(from = 0.5, to = 1, by = 0.5), 
                                     max_delta_step = 0) #don't think this param is doing anything leave at default
             
           }
           else{
             
             # paramList <- NULL
             paramList = expand.grid(eta = 10 ^ - seq(from = 2, to = 4, by = 1),
                                     max.depth = 1:5,
                                     nrounds = 5000,
                                     subsample = c(0.7, 0.8),
                                     colsample_bytree = 1,
                                     # lambda = seq(from = 0, to = 1, by = 0.2),
                                     # alpha = seq(from = 0, to = 1, by = 0.2), 
                                     max_delta_step = 0) #don't think this param is doing anything leave at default
           }
           outputData <- parallelCVxgb(inputData = trainDataSplit,
                                       k = K,
                                       trainTargets = "EventIds",
                                       paramList = paramList,
                                       Model= "XGB",
                                       testDataSplit = testDataSplit)
  # browser()
           finalPreds <- outputData$testDataSplit$predsTest
             finalModel <- outputData$trainData$predsTrain
             fittnessCV <- outputData
         },
         "RF" = {
           source("r/models/parallelCV6.R")
           
           if(codeTest){
             paramList <-  expand.grid(mtry = seq(from = 1, to = 2, by = 1),
                                       ntree = seq(from = 1000, to = 1200, by = 100),
                                       nodesize = seq(from = 1, to = 2, by = 1))#,
           }
           else{
             paramList <-  expand.grid(mtry = seq(from = 1, to = 12, by = 1),
                                       ntree = seq(from = 1000, to = 2400, by = 200),
                                       nodesize = seq(from = 1, to = 10, by = 1))
             
           }
           fittnessCV <- parallelCV(trainDataSplit,
                                    k = K,
                                    paramList = paramList,
                                    Model="RF",
                                    Parallel = Parallel,
                                    Cores = Cores,
                                    printSummary = printSummary)
           finalModel <- do.call("randomForest",
                                 c(list(x = trainDataSplit[, !names(trainDataSplit) == "EventIds"],
                                        y = factor(trainDataSplit$EventIds)),
                                   fittnessCV$bestParams,
                                   importance = TRUE))
           
           #            print(finalModel$confusion)
           if(printSummary){
             plot(finalModel)
           }
           finalPreds <- predict(finalModel,
                                 newdata=testDataSplit[, !names(testDataSplit) == "EventIds"],
                                 type="response")
           
           finalFittness <- fittnessFun(obs = testDataSplit$EventIds,
                                        pred = finalPreds)
           if(printSummary){
             varImpPlot(finalModel)
           }
           
         })
  
  
  ######print out the results#####
  
  propTable <- table(finalPreds)
  if(printSummary){
    print(propTable)
    cat("Sensitivity == Recall, PPV == Precision\n")
  }
  #fScore<- 2 * confusionMat$byClass[, "Sensitivity"] * confusionMat$byClass[, "Pos Pred Value"] /
   # (confusionMat$byClass[, "Sensitivity"] + confusionMat$byClass[, "Pos Pred Value"])
  #if(printSummary){
    
   # cat("####f Score:\n")
  #  print(data.frame(fScore))
    #   cat("Out of sample accuracy:",
    #       sum(testDataSplit$EventIds == finalPreds)/length(finalPreds), "\n")
  #}
  # browser()
  outputData <- list(finalModel = finalModel,
                     #confusionMat = confusionMat,
                     #fittnessCV = fittnessCV$meanfittnessCV,
                     finalPreds = finalPreds,
                     trainDataSplit = fittnessCV$trainData,
                     testDataSplit = fittnessCV$testDataSplit,
                     # trainTestIdx = trainTestIdx,
                     bestParams = fittnessCV$bestParams,
                     #fScore = fScore,
                     Dummies = Dummies,
                     Model = Model)
  if(Model == "SVM"){
    outputData$kernel <- Kernel
  }
  
  if(SAVE){
    if(Model == "SVM"){
      fName <- paste0(Model, " Model with ",Kernel," kernel, ", K, "-fold CV, on ", Sys.Date(),".RData")
    }else{
      fName <- paste0(substr(SealNames[k],12,nchar(SealNames[k])-8)," Test, ","Hz = ", Hz,", epochs = ", epochs,".RData")
    }
    cat("##### saving", fName, "\n")
    
    #mainDir <- getwd()
    if(codeTest){
      subDir <- "I:/Juvenile fur seal energetics/output/outputDirectory/test/"
    }
    else{
      subDir <- "I:/Juvenile fur seal energetics/output/outputDirectory/"
    }
    #if (file.exists(subDir)){
    #  setwd(file.path(mainDir, subDir))
    #} else {
      #dir.create(file.path(mainDir, subDir))
    #  setwd(file.path(mainDir, subDir))
    #}
    save(outputData, file=paste0(subDir,fName))
    #setwd(mainDir)   
  }
  return(outputData)  
}
