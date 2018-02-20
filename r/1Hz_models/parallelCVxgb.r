parallelCVxgb <- function(inputData,
                          k = 5,
                          paramList = NULL,
                          trainTargets = "EventIds",
                          testDataSplit = NULL,
                          plotCvScore = TRUE,
                          preds = FALSE,
                          folds_list = NULL,
                          nthreads = 1
                          ){
  
  library("xgboost")
  numClass <- length(unique(inputData[, trainTargets]))
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
    xgbModel <- xgb.cv(params = list(objective = "multi:softprob",
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
                       nthread = nthreads,
                       nfold = k,
                       data = dtrain,
                       nrounds = paramList$nrounds[i],
                       folds = folds_list,
                       eval_metric = "merror",
                       early.stop.round = 250,
                       print.every.n = 250,
                       prediction = FALSE,
                       showsd = TRUE,
                       stratified = TRUE,
                       verbose = TRUE,
                       maximize = FALSE)
    # browser()
    ##### find the best paramters
    paramList$acc[i] = 1-min(xgbModel$test.merror.mean)
    paramList$boostingItt[i] = which.min(xgbModel$test.merror.mean)
    paramList$std[i] <- xgbModel$test.merror.std[paramList$boostingItt[i]]
    
    if(plotCvScore){
      library(ggplot2)
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
    dtest <- xgb.DMatrix(data = data.matrix(testDataSplit[, !idx]),
                         label = as.numeric(testDataSplit[, idx]) - 1)
  }
  set.seed(12345)
  xgbModel <- xgb.train(params = list(objective = "multi:softprob",
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
    probTest <- as.data.frame(matrix(predict(object = xgbModel,
                                             newdata = dtest), ncol = numClass,
                                     byrow = TRUE))
    
    names(probTest) <- levels(inputData[, idx])
    testDataSplit$prob <- probTest
    
    testDataSplit$predsTest <- levels(inputData[, idx])[max.col(probTest)]
      
  }
  
  probTrain <- as.data.frame(matrix(predict(object = xgbModel,
                                           newdata = dtrain), ncol = numClass,
                                   byrow = TRUE))
  
  names(probTrain) <- levels(inputData[, idx])
  inputData$prob <- probTrain
  
  inputData$predsTrain <- levels(inputData[, idx])[max.col(probTrain)]
  
  # inputData$predsTrain = levels(inputData[, idx])[predict(object = xgbModel, newdata = dtrain) + 1]
  
  ##### train metrics
  cat("\n##### train metrics \n")
  confusionMatrixTrain <- confusionMatrix(data = inputData$predsTrain,
                                          reference = inputData$EventIds)
  print(confusionMatrixTrain)
  
  
  if(!is.null(testDataSplit)){
    cat("\n##### test metrics \n")
    confusionMatrixTest <- confusionMatrix(data = testDataSplit$predsTest,
                        reference = testDataSplit$EventIds)
    print(confusionMatrixTest)
    print(diag(confusionMatrixTest$table)/colSums(confusionMatrixTest$table))
    cat("Sensitivity == Recall, PPV == Precision\n")
    
    return(list(xgbModel = xgbModel,
                bestParams = bestParams,
                paramList = paramList,
                trainData = inputData,
                testDataSplit = testDataSplit,
                confusionMatrixTrain = confusionMatrixTrain, 
                confusionMatrixTest = confusionMatrixTest))
  }else{
    return(list(xgbModel = xgbModel,
                bestParams = bestParams,
                paramList = paramList,
                trainData = inputData,
                testDataSplit = testDataSplit,
                confusionMatrixTrain = confusionMatrixTrain))
  }
}