ml_run <- function(trainData,
                   testData,
                   folds_list = NULL,
                   Model = "XGB",
                   codeTest = TRUE,
                   Parallel = TRUE,
                   K = 10,
                   SAVE = TRUE,
                   Cores = 1,
                   Kernel = "linear",
                   printSummary = TRUE){
  
  library(caret)
  
  ##### creaete a function to evaluate the final fittness
  fittnessFun <- function(obs,pred){
    ACC <- sum(obs==pred)/length(pred)
  }
  
  switch(Model,
         "XGB" = {
           #source("parallelCVxgb.r")
           if(codeTest){
             paramList = expand.grid(eta = 10 ^ - seq(from = 1, to = 1, by = 1),
                                     max.depth = 1:2,
                                     nrounds = 2,
                                     subsample = c(0.5),
                                     colsample_bytree = 0.5,
                                     # lambda = seq(from = 0.5, to = 1, by = 0.5),
                                     # alpha = seq(from = 0.5, to = 1, by = 0.5), 
                                     max_delta_step = 0) #don't think this param is doing anything leave at default
             
           }
           else{
             
             # paramList <- NULL
             paramList = expand.grid(eta = 10 ^ - seq(from = 2, to = 4, by = 1),
                                     max.depth = 2:5,
                                     nrounds = 5000,
                                     subsample = c(0.7, 0.8),
                                     colsample_bytree = c(0.7, 0.8),
                                     # lambda = seq(from = 0, to = 1, by = 0.2),
                                     # alpha = seq(from = 0, to = 1, by = 0.2), 
                                     max_delta_step = 0) #don't think this param is doing anything leave at default
           }
           outputData <- parallelCVxgb(inputData = trainData,
                                       folds_list = folds_list,
                                       k = K,
                                       trainTargets = "EventIds",
                                       paramList = paramList,
                                       testDataSplit = testData,
                                       nthreads = Cores)

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
  
  ##### calculate the f-score
  fScore <- 2 * outputData$confusionMatrixTest$byClass[, "Sensitivity"] *
    outputData$confusionMatrixTest$byClass[, "Pos Pred Value"] /
    (outputData$confusionMatrixTest$byClass[, "Sensitivity"] +
       outputData$confusionMatrixTest$byClass[, "Pos Pred Value"])
  
  ######print out the results#####
  if(printSummary){
    
    cat("####f Score:\n")
    print(data.frame(fScore))
    test_accuary <- sum(testData$EventIds == outputData$testDataSplit$predsTest)/length(outputData$testDataSplit$predsTest)
    cat("Out of sample accuracy:", round(test_accuary * 1000) / 1000, "\n")
  }
  # browser()
  outputData$fScore <- fScore
  outputData$Model <- Model
  outputData$test_accuary <- test_accuary
  
  
  return(outputData)  
}
