parallelCV <- function(inputData,
                       k=5,
                       paramList = NULL,
                       Model="GBM",
                       Fittness="ACC",
                       trainTargets="EventIds",
                       Cores=4,
                       Parallel=TRUE,
                       printSummary){
  
  # library("caret")
  #   library("dplyr")
  # library(doMC)
  registerDoMC(cores = Cores)
  set.seed(12345)
  nSeeds <- nrow(paramList %>% select(shrinkage, interaction.depth, n.minobsinnode) %>% 
                   group_by(shrinkage, interaction.depth, n.minobsinnode))
  
  Seeds <- vector(mode = "list", length = k + 1)
  for(i in 1:k) Seeds[[i]] <- sample.int(1000, nSeeds) # Why 22?
  
  ## For the last model:
  Seeds[[k+1]] <- sample.int(1000, 1)
  
  fitControl <- trainControl(method = "cv",
                             number = k,
                             allowParallel = Parallel,
                             seeds = Seeds)
  
  ##### create the cross validation folds
  folds <- createFolds(inputData[, trainTargets], k=k)
  xx <- inputData[, names(inputData) != trainTargets]
  yy <- factor(inputData[, trainTargets])
  
  nParma1 <- length(paramList$param1)
  nParma2 <- length(paramList$param2)
  
  #   if(is.list(paramList)){
  #     ###### insert a dummy variable into param3, need to think of a nicer way to do this
  #     # could use eval and parse, but this might be slow?
  #     paramList$param3=1
  #   }
  
  ##### results storage
  fittnessScore <- matrix(nrow=nParma1, ncol=nParma2)
  
  switch(Model,
         "GBM" = {
           # library("gbm")
           #            SealName <- inputData[, !(names(inputData) %in% "SealName")]
           inputData$EventIds <- factor(inputData$EventIds)           
#            inputData$Place <- factor(inputData$Place)           
#            inputData$Gender <- factor(inputData$Gender)           
#            inputData$species <- factor(inputData$species)           
#            inputData$harsness <- factor(inputData$harsness)           

           if(!exists("paramList")){
             paramList <-  expand.grid(interaction.depth = seq(from = 1, to = 2, by = 1),
                                       n.trees = seq(from = 100, to = 300, by = 200),
                                       shrinkage = 10 ^ seq(from = -1, to = -2, by = -1),
                                       n.minobsinnode = seq(from = 10, to = 11, by = 1))
           }
           modelFit <- train(EventIds ~ ., data = inputData,
                             method = "gbm",
                             trControl = fitControl,
                             verbose = FALSE,
                             ## Now specify the exact models 
                             ## to evaludate:
                             tuneGrid = paramList)
           
         })
  #   meanfittnessCV <- data.frame(Reduce("+", fittnessCV)/k, row.names = paramList$param1)
  #   names(meanfittnessCV) <- paramList$param2
  if(printSummary){
    
    print("mean CV fittness:")
    print(modelFit)
    #   sdfittnessCV <- data.frame(apply(simplify2array(fittnessCV), 1:2, sd), row.names = paramList$param1)
    #   names(sdfittnessCV) <- paramList$param2
    #   print("sd CV fittness:")
    #   print(sdfittnessCV)
    
    cat("the best parameters: \n")
    print(modelFit$bestTune)
  }
  meanfittnessCV = modelFit$results[,c(1:5,7)]
  names(meanfittnessCV)[5:6] <- c("fittness", "sd")
  
  return(list(meanfittnessCV = meanfittnessCV,
              bestParams = modelFit$bestTune))
  
}