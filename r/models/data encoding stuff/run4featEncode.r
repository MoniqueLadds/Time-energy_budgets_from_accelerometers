##### run throught the behaviour coding and feature processing
# 4 behaviour
# add the 4 behaviour information
behaviourCodeLoop(PlotBeahviour = F, nBehaviours = 4)

# process the data for modeling
source('~/Documents/r_files/Seals/featureProcessingTime.r')
# 75HZ
featData4_75=featureProcessing(nBehaviours = 4, targetCol = "type_event75")
save(featData4_75, file = "featData4_75v2.RData")
corMat <- cor(featData4_75[, sapply(featData4_75, class) == "numeric"])
max(abs(corMat[upper.tri(corMat)]))

# 25
featData4_25=featureProcessing(nBehaviours = 4, targetCol = "type_event25")
save(featData4_25, file = "featData4_25v2.RData")

# 13
featData4_13=featureProcessing(nBehaviours = 4, targetCol = "type_event13")
save(featData4_13, file = "featData4_13v2.RData")

##### above has been run
# 11
featData4_11=featureProcessing(nBehaviours = 4, targetCol = "type_event11")
save(featData4_11, file = "featData4_11v2.RData")

# 9
featData4_9=featureProcessing(nBehaviours = 4, targetCol = "type_event9")
save(featData4_9, file = "featData4_9v2.RData")

# 7
featData4_7=featureProcessing(nBehaviours = 4, targetCol = "type_event7")
save(featData4_7, file = "featData4_7v2.RData")

# 5
featData4_5=featureProcessing(nBehaviours = 4, targetCol = "type_event5")
save(featData4_5, file = "featData4_5.RData")

# 3
featData4_3=featureProcessing(nBehaviours = 4, targetCol = "type_event3")
save(featData4_3, file = "featData4_3.RData")
