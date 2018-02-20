parallelCV <- function(inputData,
                       k=5,
                       paramList = NULL,
                       Model="RF",
                       Fittness="ACC",
                       trainTargets="EventIds",
                       Cores=4,
                       Parallel=TRUE,
                       printSummary){
  
  #   library("caret")
  #   library("dplyr")
  
  ##### create the cross validation folds
  folds <- createFolds(inputData[, trainTargets], k=k)
  xx <- inputData[, names(inputData) != trainTargets]
  yy <- factor(inputData[, trainTargets])
  yyNum <- as.numeric(yy)
  
  ##### results storage
  #   fittnessScore <- matrix(nrow=nParma1, ncol=nParma2)
  
  switch(Model,
         "RF"={
           #            xx <- inputData[, names(inputData) != trainTargets]
           #            yy <- factor(inputData[, trainTargets])
           #            library("randomForest")
           #            Packages <- "randomForest"
           ##### define model training function
           
           sampSize = ceiling(.632*nrow(xx))
           
           trainFunc <- function(params, cv, nr){
             modelFit <- do.call("randomForest", c(list(x = xx[-folds[[cv]],],
                                                        y = yy[-folds[[cv]]]),
                                                   params,
                                                   cutoff = c(0.25, 0.25, 0.25, 0.25),
                                                   proximity = FALSE,
                                                   oob.prox = FALSE,
                                                   keep.forest = TRUE,
                                                   sampsize = sampSize))
             sum(yy[folds[[cv]]] == randomForest:::predict.randomForest(modelFit,
                                                                        newdata = xx[folds[[cv]],],
                                                                        cutoff = c(0.25, 0.25, 0.25, 0.25)))/nr
           }
           
           cvTrainFunc <- function(cv){
             nr <- length(yy[folds[[cv]]])
             apply(paramList, 1, trainFunc, cv=cv, nr=nr)
           }
           
         }, # end RF
         "SVM" = {
           # library("e1071")
           Packages <- "e1071"
           xx <- as.matrix(cbind(xx[, names(xx) != "SealName"], model.matrix(~SealName-1, data=xx)))
           trainFunc <- function(params, cv, nr){
             modelFit <- do.call("svm", c(list(x = xx[-folds[[cv]],], y = yy[-folds[[cv]]]), params))
             sum(yy[folds[[cv]]] == predict(modelFit, newdata = xx[folds[[cv]],]))/nr
           }
           
           cvTrainFunc <- function(cv){
             nr <- length(yy[folds[[cv]]])
             apply(paramList, 1, trainFunc, cv=cv, nr=nr)
           }
           
         },        
         "GBM" = {
           # library("gbm")
           Packages <- "gbm"
           #            xx <- as.matrix(cbind(xx[, names(xx) != "SealName"], model.matrix(~SealName-1, data=xx)))
           #            cat("hear")
           
           behaviourLevels <- levels(yy)
           
           tempParams <- paramList %>% filter(n.trees == max(n.trees))
           predTrees = paramList %>% 
             select(n.trees) %>%
             distinct()
           
           trainFunc <- function(params, cv, nr, predTrees){
             modelFit <- do.call("gbm.fit", c(list(x = xx[-folds[[cv]],], 
                                                   y = yy[-folds[[cv]]]),
                                              params,
                                              distribution = "multinomial",
                                              verbose= FALSE))
             
             tempPreds <- predict(modelFit,
                                  newdata = xx[folds[[cv]],],
                                  n.trees = predTrees,
                                  type = "response")
             #              sapply(1:length(predTrees),
             #                     function(jj){
             #                       sum(yyNum[folds[[cv]]] == max.col(tempPreds[,,jj]))/nr
             #                     })
             
             sapply(1:length(predTrees),
                    function(jj){
                      sum(yy[folds[[cv]]] ==
                            sapply(max.col(tempPreds[,,jj]),
                                   function(ii){behaviourLevels[ii]}))/nr
                    }) # end jj, end outer sapply
           }
           
           cvTrainFunc <- function(cv){
             nr <- length(yy[folds[[cv]]])
             c(apply(tempParams, 1, trainFunc, cv=cv, nr=nr,
                     predTrees$n.trees))
           }
         }
         
  ) #end switch
  
  if(Parallel){
    
    #     library("doParallel")
    #     cl <- makeCluster(Cores)
    #     registerDoParallel(cl)
    #     fittnessCV <- foreach(CV = 1:k,
    #                           .options.RNG = 1234,
    #                           .packages = Packages) %dorng% { cvTrainFunc(cv = CV) }
    #     
    #     stopCluster(cl)
    
    fittnessCV <- parallel::mclapply(1:k, function(CV){cvTrainFunc(cv = CV)},
                           mc.preschedule=FALSE,
                           mc.cores=Cores)
    
  }else{
    fittnessCV <- lapply(1:k, function(CV){cvTrainFunc(cv=CV)})    
  }
  
  meanfittnessCV <- data.frame(paramList,
                               fittness = matrix(Reduce("+", fittnessCV)/k, ncol=1),
                               sd = matrix(apply(simplify2array(fittnessCV), 1, sd), ncol=1))
  bestParams = paramList[which.max(meanfittnessCV$fittness),]
  
  if(printSummary){
    
    cat("CV fittness: \n")
    print(meanfittnessCV)
    
    cat("the best parameters are: \nx")
    print(bestParams)
  }
  return(list(meanfittnessCV=meanfittnessCV,
              bestParams = bestParams))
}