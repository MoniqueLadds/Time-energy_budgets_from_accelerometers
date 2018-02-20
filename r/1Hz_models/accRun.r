accRun <- function(featureData,
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
    featureData$Place <- as.numeric(as.factor(featureData$Place))   
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
  
  ##### remove the indetifier variables
  trainData <- sampledData[, !(names(sampledData) %in% c("FileDate", "SealName", "nRows", "X"))]
  
  if(codeTest){
    ###### use this for testing for fast model training
    trainTestIdx <- createDataPartition(y=trainData$EventIds, times=1, p = 0.2)
  }else{
    ##### full data training and testing
    trainTestIdx <- createDataPartition(y=trainData$EventIds, times=1, p = 0.7)
  }
  trainDataSplit=trainData[trainTestIdx$Resample1, ]
  testDataSplit=trainData[-trainTestIdx$Resample1, ]
  
  if(printSummary){
    cat("##### training split data summary:\n")
    print(table(trainDataSplit$EventIds))
    cat("##### testing split data summary:\n")
    print(table(testDataSplit$EventIds))
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
             paramList = expand.grid(eta = 10 ^ - seq(from = 1, to = 2, by = 1),
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
                                       #Model= "XGB",
                                       testDataSplit = testDataSplit)
           # browser()
           finalPreds <- outputData$testDataSplit$predsTest
             finalModel <- outputData$trainData$predsTrain
             fittnessCV <- outputData
         },
         "rPart" = {
           ##### model for plotting only
           require("rpart")
           require("ggplot2")
           fittnessCV = NULL
           
           finalModel <- rpart(EventIds ~., method="class", data=trainDataSplit)
           finalPreds <- predict(finalModel,
                                 newdata=testDataSplit[, !names(testDataSplit) == "EventIds"],
                                 type="class")
           prp(finalModel, extra=0,
               box.col=c(5:8)[finalModel$frame$yval])
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
           
         },
         "SVM" = {
           source("parallelCVSvm.r")
           
           if(codeTest){
             #              degree  
             #              parameter needed for kernel of type polynomial (default: 3)
             #              
             #              gamma	
             #              parameter needed for all kernels except linear (default: 1/(data dimension))
             #              
             #              coef0	
             #              parameter needed for kernels of type polynomial and sigmoid (default: 0)
             #              
             #              cost	
             #              cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation.
             
             switch(Kernel,
                    "linear" = {
                      paramList <-  data.frame(cost = 10 ^ seq(from = -5, to = 0, by = 5))
                    },
                    "polynomial" = {
                      paramList <-  expand.grid(degree = seq(from = 0, to = 2, by = 1),
                                                gamma = 10 ^ seq(from = -5, to = 0, by = 5),
                                                coef0 = seq(from = 0, to = 1, by = 1),
                                                cost = 10 ^ seq(from = -5, to = 0, by = 5))
                    },
                    "radial" = {
                      paramList <-  expand.grid(gamma = 10 ^ seq(from = -5, to = 0, by = 5),
                                                cost = 10 ^ seq(from = -5, to = 0, by = 5))
                    },
                    "sigmoid" = {
                      paramList <-  expand.grid(gamma = 10 ^ seq(from = -5, to = 0, by = 5),
                                                coef0 = seq(from = 0, to = 1, by = 1),
                                                cost = 10 ^ seq(from = -5, to = 0, by = 5))
                    }
             )
             
           }else{
             switch(Kernel,
                    "linear" = {
                      paramList <-  data.frame(cost = 10 ^ seq(from = -5, to = 5, by = 1))
                    },
                    "polynomial" = {
                      paramList <-  expand.grid(degree = seq(from = 0, to = 5, by = 1),
                                                gamma = 10 ^ seq(from = -4, to = 5, by = 2),
                                                coef0 = seq(from = 0, to = 5, by = 1),
                                                cost = 10 ^ seq(from = -4, to = 5, by = 2))
                    },
                    "radial" = {
                      paramList <-  expand.grid(gamma = 10 ^ seq(from = -5, to = 5, by = 1),
                                                cost = 10 ^ seq(from = -5, to = 5, by = 1))
                    },
                    "sigmoid" = {
                      paramList <-  expand.grid(gamma = 10 ^ seq(from = -4, to = 5, by = 2),
                                                coef0 = seq(from = 0, to = 5, by = 1),
                                                cost = 10 ^ seq(from = -4, to = 5, by = 2))                    }
             )             
           }
           fittnessCV <- parallelCVSvm(trainDataSplit, 
                                       k = K,
                                       paramList = paramList,
                                       Model="SVM",
                                       Parallel = Parallel,
                                       Cores = Cores,
                                       Dummies = Dummies,
                                       Kernel = Kernel)
           
           xx <- trainDataSplit[, !names(trainDataSplit) == "EventIds"]
           if(Dummies){
             xx <- as.matrix(cbind(xx[, !(names(xx) %in% c("Place", "Gender", "species", "harsness"))],
                                   model.matrix(~Place + Gender + species + harsness-1, data=xx))) 
           }else
           {
             xx <- as.matrix(xx)
           }
           
           
           finalModel <- do.call("svm",
                                 c(list(x = xx,
                                        y = factor(trainDataSplit$EventIds)),
                                   fittnessCV$bestParams,
                                   kernel = Kernel,
                                   type = "C-classification"))
           
           xxTest <- testDataSplit[, !names(testDataSplit) == "EventIds"]
           if(Dummies){
             xxTest <- as.matrix(cbind(xxTest[, !(names(xxTest) %in% c("Place", "Gender", "species", "harsness"))],
                                       model.matrix(~Place + Gender + species + harsness-1, data=xxTest)))  
           }else
           {
             xxTest <- as.matrix(xxTest)
           }
           
           finalPreds <- predict(finalModel,
                                 newdata=xxTest,
                                 type="response")
           
           finalFittness <- fittnessFun(obs = testDataSplit$EventIds,
                                        pred = finalPreds)
           
         },
         "RLR"={
           source("parallelCvRlr.r")
           
           if(codeTest){
             paramList = list(param1=seq(from = 0, to = 1, by = 0.5),
                              param2=10^seq(from = 0, to = -3, length.out = 5))
             
           }
           else{
             
             paramList = list(param1=seq(from = 0, to = 1, by = 0.01),
                              param2=10^seq(from = 0, to = -3, length.out = 50))
           }
           ##### train the model to find the best parameters
           fittnessCV <- parallelCvRlr(trainDataSplit,
                                       k = K,
                                       paramList = paramList,
                                       Model="RLR",
                                       Parallel = FALSE,
                                       Cores = Cores,
                                       Dummies = Dummies,
                                       printSummary = printSummary)
           
           xx <- trainDataSplit[, !names(trainDataSplit) == "EventIds"]
           
           if(Dummies){
             xx <- as.matrix(cbind(xx[, !(names(xx) %in% c("Place", "Gender", "species", "harsness"))],
                                   model.matrix(~Place + Gender + species + harsness -1, data=xx)))
           }else
           {
             xx <- as.matrix(xx)
           }
           ##### re-train the overall model
           finalModel <- glmnet(x=xx,
                                y=factor(trainDataSplit$EventIds),
                                family = "multinomial",
                                alpha = fittnessCV$bestParams[1],
                                lambda = paramList$param2)
           
           #            print(finalModel)
           
           plot(finalModel)
           
           xxTest <- testDataSplit[, !names(testDataSplit) == "EventIds"]
           if(Dummies){
             xxTest <- as.matrix(cbind(xxTest[, !(names(xxTest) %in% c("Place", "Gender", "species", "harsness"))],
                                       model.matrix(~Place + Gender + species + harsness -1, data=xxTest)))
           }else
           {
             xxTest <- as.matrix(xxTest)
           }
           finalPreds <- predict(finalModel,
                                 newx=xxTest,
                                 type="class",
                                 s = fittnessCV$bestParams[2])
           
           finalFittness <- fittnessFun(obs = testDataSplit$EventIds,
                                        pred = finalPreds)
           
           
           coef(finalModel, s=fittnessCV$bestParams[2])
           
           
         },
         "GBM" = {
           source("parallelCVGBM.r")
           
           if(codeTest){
             paramList <-  expand.grid(n.trees = seq(from = 30, to = 45, by = 5),
                                       interaction.depth = seq(from = 1, to = 3, by = 2),
                                       shrinkage = 10 ^ seq(from = -1, to = -2, by = -1),
                                       n.minobsinnode = seq(from = 7, to = 11, by = 2))
             
           }
           else{
             
             paramList <-  expand.grid(interaction.depth = seq(from = 1, to = 6, by = 2),
                                       n.trees = seq(from = 800, to = 2400, by = 100),
                                       shrinkage = 10 ^ seq(from = -1, to = -3, by = -1),
                                       n.minobsinnode = seq(from = 1, to = 17, by = 2))
             
             #              paramList <-  expand.grid(interaction.depth = seq(from = 1, to = 17, by = 2),
             #                                        n.trees = seq(from = 1200, to = 2400, by = 200),
             #                                        shrinkage = 10 ^ seq(from = -1, to = -3, by = -1),
             #                                        n.minobsinnode = seq(from = 1, to = 17, by = 2))
           }
           
           ##### train the model
           fittnessCV <- parallelCV(trainDataSplit,
                                    k = K,
                                    paramList = paramList,
                                    Model="GBM",
                                    Parallel = Parallel,
                                    Cores = Cores,
                                    printSummary = printSummary)
           
           if(Dummies){
             trainDataSplit$Place <- factor(trainDataSplit$Place)           
             trainDataSplit$Gender <- factor(trainDataSplit$Gender)           
             trainDataSplit$species <- factor(trainDataSplit$species)           
             trainDataSplit$harsness <- factor(trainDataSplit$harsness)  
             testDataSplit$Place <- factor(testDataSplit$Place)           
             testDataSplit$Gender <- factor(testDataSplit$Gender)           
             testDataSplit$species <- factor(testDataSplit$species)           
             testDataSplit$harsness <- factor(testDataSplit$harsness)  
           }
           
           ##### train the final overall model at the best parameters
           finalModel <- do.call("gbm.fit",
                                 c(list(x = trainDataSplit[, !names(trainDataSplit) == "EventIds"],
                                        y = factor(trainDataSplit$EventIds)),
                                   fittnessCV$bestParams,
                                   distribution = "multinomial",
                                   verbose= FALSE))
           
           finalPreds <- predict(finalModel,
                                 newdata = testDataSplit[, !names(testDataSplit) == "EventIds"],
                                 n.trees = fittnessCV$bestParams$n.trees,
                                 type = "response")[,,1]
           
           behaviourNames <- colnames(finalPreds)
           
           finalPreds <-  sapply(max.col(finalPreds), function(ii){behaviourNames[ii]})
           
           
         })
  
  confusionMat <- confusionMatrix(data = finalPreds, reference = testDataSplit$EventIds)
  if(printSummary){
    print(confusionMat)
    cat("Sensitivity == Recall, PPV == Precision\n")
  }
  fScore<- 2 * confusionMat$byClass[, "Sensitivity"] * confusionMat$byClass[, "Pos Pred Value"] /
    (confusionMat$byClass[, "Sensitivity"] + confusionMat$byClass[, "Pos Pred Value"])
  if(printSummary){
    
    cat("####f Score:\n")
    print(data.frame(fScore))
    #   cat("Out of sample accuracy:",
    #       sum(testDataSplit$EventIds == finalPreds)/length(finalPreds), "\n")
  }
  
  outputData <- list(finalModel = finalModel,
                     confusionMat = confusionMat,
                     #fittnessCV = fittnessCV$meanfittnessCV,
                     finalPreds = finalPreds,
                     trainDataSplit = trainDataSplit,
                     testDataSplit = testDataSplit,
                     trainTestIdx = trainTestIdx,
                     bestParams = fittnessCV$bestParams,
                     fScore = fScore,
                     Dummies = Dummies,
                     Model = Model)
  if(Model == "SVM"){
    outputData$kernel <- Kernel
  }
  
  if(SAVE){
    if(Model == "SVM"){
      fName <- paste0(Model, " Model with ",Kernel," kernel, ", K, "-fold CV, on ", Sys.Date(),".RData")
    }else{
      fName <- paste0(Model, " Model, ","Hz = ", Hz,", epochs = ", epochs,".RData")
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
