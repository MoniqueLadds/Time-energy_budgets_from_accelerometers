parallelCvRlr <- function(inputData,
                          k=5,
                          paramList = NULL,
                          Model="RLR",
                          Fittness="ACC",
                          trainTargets="EventIds",
                          Cores=4,
                          Parallel = FALSE,
                          Dummies = TRUE,
                          printSummary = TRUE){
  
  # library("caret")
  #   library("dplyr")
  fitControl <- trainControl(method = "cv",
                             number = k,
                             allowParallel = Parallel)
  
  ##### create the cross validation folds
  folds <- createFolds(inputData[, trainTargets], k=k)
  xx <- inputData[, names(inputData) != trainTargets]
  yy <- factor(inputData[, trainTargets])
  
  nParma1 <- length(paramList$param1)
  nParma2 <- length(paramList$param2)
  
  
  ##### results storage
  fittnessScore <- matrix(nrow=nParma1, ncol=nParma2)
  
  switch(Fittness,
         "AUC"={
           ##### define ROC function
           fittnessFun <- function(obs,pred){
             n1 <- sum(obs)
             n <- length(obs)
             ROC <- (mean(rank(pred)[obs == 1]) - (n1 + 1)/2)/(n - n1)
           }
         },
         "ACC"={
           fittnessFun <- function(pred, obs){
             ACC <- sum(obs==pred)/length(pred)
           }
           
         })
  
  switch(Model,
         "RLR"={
           ###### settup model matrix
           xx <- inputData[, names(inputData) != trainTargets]
           yy <- factor(inputData[, trainTargets])
           if(Dummies){
             xx <- as.matrix(cbind(xx[, !(names(xx) %in% c("Place", "Gender", "species", "harsness"))],
                                   model.matrix(~Place + Gender + species + harsness -1, data=xx)))
           }
           else{
             xx <- as.matrix(xx)
             
           }
           # library("glmnet")
           modelFun <- function(xx, yy, param1, param2){
             # browser()
             
             Model <- glmnet(x = xx, y = yy, family="multinomial",
                             alpha=param1, lambda=param2)
           }
           predFun <- function(Model, xx){
             predict(Model,
                     newx=xx,
                     type="class")
           }  
         }
  )
  
  cvInnerLoops <- function(xx, yy, paramList, folds, cv, Model){
    
    switch(Model,
           ##### for regularised logistic regression it is 
           ##### faster to have the glmnet package compute 
           ##### the entire lambda path in on function call
           "RLR" = {
             for(i in 1:nParma1){
               Model <- modelFun(xx[-folds[[cv]],],
                                 yy[-folds[[cv]]],
                                 ###### param1 = alpha
                                 param1 = paramList$param1[i],
                                 ###### param1 = lambda
                                 param2 = paramList$param2)
               
               ###### have to loop over prediction function for each lambda value
               ###### should replace this with sapply
               #                for(j in 1:nParma2){
               #                fittnessScore[i, j] <- fittnessFun(yy[folds[[cv]]],
               #                                                   predFun(Model,
               #                                                           xx[folds[[cv]],])
               #                )
               ###### predict the entire lambda path in one pass
               preds <- predict(Model,
                                newx=xx[folds[[cv]],],
                                type="class")
               #                cat(i,"\n")
               # if(i==8){browser()}
               temp <- apply(preds, 2, fittnessFun, obs=yy[folds[[cv]]])
               fittnessScore[i,1:length(temp)] <- temp
               
               
               #                } #end j
             } # end i
           } # end RLR
    ) #end switch
    return(fittnessScore)
  } # end cvInnerLoops
  
  if(Parallel){
    fittnessCV <- mclapply(1:k, function(CV){cvInnerLoops(xx=xx, yy=yy, paramList=paramList, folds=folds, cv=CV, Model=Model)},
                           mc.preschedule=FALSE,
                           mc.cores=Cores)
  }else{
    fittnessCV <- lapply(1:k, function(CV){cvInnerLoops(xx=xx, yy=yy, paramList=paramList, folds=folds, cv=CV, Model=Model)})
  }
  meanfittnessCV <- data.frame(Reduce("+", fittnessCV)/k, row.names = paramList$param1)
  names(meanfittnessCV) <- paramList$param2
  sdfittnessCV <- data.frame(apply(simplify2array(fittnessCV), 1:2, sd), row.names = paramList$param1)
  names(sdfittnessCV) <- paramList$param2
  bestParams = arrayInd(which.max(as.matrix(meanfittnessCV)), .dim = dim(meanfittnessCV))
  
  if(printSummary){
    
    print("mean CV fittness:")
    print(meanfittnessCV)
    print("sd CV fittness:")
    print(sdfittnessCV)
    cat("the best parameters: \n")
    print(c(paramList$param1[bestParams[1]], paramList$param2[bestParams[2]]))
  }
  return(list(meanfittnessCV=meanfittnessCV, sdfittnessCV=sdfittnessCV,
              bestParams= c(paramList$param1[bestParams[1]], paramList$param2[bestParams[2]])))
}