parallelCVxgb <- function(inputData,
                          k = 5,
                          paramList = NULL,
                          Model = "xgb",
                          trainTargets = "EventIds",
                          testDataSplit = NULL,
                          plotCvScore = TRUE,
                          preds = FALSE){
  
  # library("dplyr")
  numClass <- 4
  dataNames <- names(inputData)
  idx <- dataNames %in% trainTargets
  
  inputData[, idx] <- factor(inputData[, idx])
  
#   inputData$place <- as.numeric(inputData$place)
#   inputData$Gender <- as.numeric(inputData$Gender)
#   inputData$species <- as.numeric(inputData$species)
#   inputData$harsness <- as.numeric(inputData$harsness)
  dtrain <- xgb.DMatrix(data = data.matrix(inputData[, !idx]),
                        label = as.numeric(factor(inputData[, idx])) - 1)
  set.seed(1234)
  
  ##### append paramList for summary stats storage
  paramList$acc <- 0
  paramList$boostingItt <- 0
  
  ##### loop over param list
  for(i in 1:nrow(paramList)){
    xgbModel <- xgb.cv(params = list(objective = "multi:softmax",
                                     eta = paramList$eta[i],
                                     max.depth = paramList$max.depth[i],
                                     subsample = paramList$subsample[i],
                                     colsample_bytree = paramList$colsample_bytree[i],
                                     lambda = paramList$lambda[i],
                                     alpha = paramList$alpha[i],
                                     max_delta_step = paramList$max_delta_step[i],
                                     num_class = numClass,
                                     min_child_weight = paramList$min_child_weight[i],
                                     gamma = paramList$gamma[i]),
                       data = dtrain,
                       nrounds = paramList$nrounds[i],
                       nfold = k,
                       eval_metric = "merror",
                       # early.stop.round = 250,
                       print.every.n = 250,
                       prediction = FALSE,
                       showsd = TRUE,
                       stratified = TRUE,
                       verbose = TRUE,
                       maximize = FALSE)
    
    ##### find the best paramters
    paramList$acc[i] = 1-min(xgbModel$test.merror.mean)
    paramList$boostingItt[i] = which.min(xgbModel$test.merror.mean)
    
    if(plotCvScore){
      
      plotData = xgbModel
      plotData$boostingRound <- 1:nrow(plotData)
      pp <- ggplot() + 
        geom_line(data = plotData, aes(x = boostingRound, y = test.merror.mean), colour = "red") +
        geom_line(data = plotData, aes(x = boostingRound, y = train.merror.mean))
      print(pp)
      cat("\n")
      print(paramList[i,])
    }
    
  }
  # browser()
  
  ##### retrain the model at the best parameters
  bestParams <- paramList[which.max(paramList$acc),]
  cat("##### best params #####\n")
  print(bestParams)
  
  if(!is.null(testDataSplit)){
    testDataSplit[, idx] <- factor(testDataSplit[, idx])
    #     testDataSplit$place <- as.numeric(testDataSplit$place)
#     testDataSplit$Gender <- as.numeric(testDataSplit$Gender)
#     testDataSplit$species <- as.numeric(testDataSplit$species)
#     testDataSplit$harsness <- as.numeric(testDataSplit$harsness)
    # browser()
    dtest <- xgb.DMatrix(data = data.matrix(testDataSplit[, !idx]),
                         label = as.numeric(testDataSplit[, idx]) - 1)
  }
  set.seed(12345)
  xgbModel <- xgb.train(params = list(objective = "multi:softmax",
                                      eta = bestParams$eta,
                                      max.depth = bestParams$max.depth,
                                      nthread = bestParams$nthread,
                                      subsample = bestParams$subsample,
                                      colsample_bytree = bestParams$colsample_bytree,
                                      lambda = bestParams$lambda,
                                      alpha = bestParams$alpha,
                                      max_delta_step = bestParams$max_delta_step,
                                      num_class = numClass,
                                      eval_metric = "merror"),
                        data = dtrain,
                        nrounds = bestParams$boostingItt,
                        prediction = TRUE,
                        verbose = TRUE,
                        maximize = FALSE,
                        watchlist = list(train = dtrain, validation = dtest))
  
  ##### repredict at the best parameters
  ##### return the predictions at the best parameters
  ##### return CV and Validation at best parameters
  if(!is.null(testDataSplit)){
    testDataSplit$predsTest = predict(object = xgbModel, newdata = dtest) + 1
  }
  inputData$predsTrain = levels(inputData[, idx])[predict(object = xgbModel, newdata = dtrain) + 1]
  
  ##### train metrics
  cat("train metrics \n")
  confusionMatrixTrain <- confusionMatrix(data = inputData$predsTrain,
                        reference = inputData$EventIds)
  print(confusionMatrixTrain)
  
  
  if(!is.null(testDataSplit)){
   # cat("test metrics \n")
    #confusionMatrixTest <- confusionMatrix(data = testDataSplit$predsTest,
     #                     reference = testDataSplit$EventIds)
    #print(confusionMatrixTest)
    # print(diag(tmp$confusionMatrixTest$table)/colSums(tmp$confusionMatrixTest$table))
    
  return(list(xgbModel = xgbModel,
              bestParams = bestParams,
              paramList = paramList,
              trainData = inputData,
              testDataSplit = testDataSplit,
              confusionMatrixTrain = confusionMatrixTrain))
  }else{
    return(list(xgbModel = xgbModel,
                bestParams = bestParams,
                paramList = paramList,
                trainData = inputData,
                testDataSplit = testDataSplit,
                confusionMatrixTrain = confusionMatrixTrain))
  }
}