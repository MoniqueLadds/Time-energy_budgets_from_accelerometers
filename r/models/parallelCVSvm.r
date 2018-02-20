parallelCVSvm <- function(inputData,
                          k=5,
                          paramList = NULL,
                          Model = "SVM",
                          Fittness = "ACC",
                          trainTargets = "EventIds",
                          Cores = 4,
                          Parallel = TRUE,
                          Dummies = TRUE,
                          Kernel = "linear"){
  
  library("caret")
  #   library("dplyr")
  #   library(doMC)
  #   registerDoMC(cores = Cores)
  #   set.seed(1234)
  
  ##### create the cross validation folds
  folds <- createFolds(inputData[, trainTargets], k=k)
  xx <- inputData[, names(inputData) != trainTargets]
  yy <- factor(inputData[, trainTargets])
  
  ##### results storage
  #   fittnessScore <- matrix(nrow=nParma1, ncol=nParma2)
  
  switch(Model,
         "SVM" = {
           library("e1071")
           Packages <- "e1071"
           #            xx <- trainDataSplit[, !names(trainDataSplit) == "EventIds"]
           if(Dummies){
             xx <- as.matrix(cbind(xx[, !(names(xx) %in% c("Place", "Gender", "species", "harsness"))],
                                   model.matrix(~Place + Gender + species + harsness-1, data=xx)))
           }
           else{
             xx <- as.matrix(xx)
             
           }
           trainFunc <- function(params, cv, nr){
             modelFit <- do.call("svm",
                                 c(list(x = xx[-folds[[cv]],],
                                        y = yy[-folds[[cv]]]),
                                   params,
                                   kernel = Kernel,
                                   type = "C-classification"))
             sum(yy[folds[[cv]]] == predict(modelFit, newdata = xx[folds[[cv]],]))/nr
           }
           
           cvTrainFunc <- function(cv){
             nr <- length(yy[folds[[cv]]])
             apply(paramList, 1, trainFunc, cv=cv, nr=nr)
           }
           
         }
  ) #end switch
  
  if(Parallel){
    
    #     library(doParallel)
    #     cl <- makeCluster(Cores)
    #     registerDoParallel(cl)
    #     
    #     fittnessCV <- foreach(CV = 1:k,
    #                           .options.RNG = 1234,
    #                           .packages = Packages) %dorng% { cvTrainFunc(cv = CV) }
    #     
    #     stopCluster(cl)
    fittnessCV <- mclapply(1:k, function(CV){cvTrainFunc(cv=CV)},
                           mc.preschedule=FALSE,
                           mc.cores=Cores)    
    
  }else{
    fittnessCV <- lapply(1:k, function(CV){cvTrainFunc(cv=CV)})    
  }
  
  # browser()
  meanfittnessCV <- data.frame(paramList,
                               fittness = matrix(Reduce("+", fittnessCV)/k, ncol=1),
                               sd = matrix(apply(simplify2array(fittnessCV), 1, sd), ncol=1))
  cat("CV fittness: \n")
  print(meanfittnessCV)
  cat("the best parameters are: \n")
  bestParams = data.frame(paramList[which.max(meanfittnessCV$fittness),])
  names(bestParams) <- names(paramList)
  print(bestParams)
  
  return(list(meanfittnessCV=meanfittnessCV,
              bestParams = bestParams))
}
