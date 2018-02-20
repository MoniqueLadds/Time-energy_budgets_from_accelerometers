##### run throught the behaviour coding and feature processing
# 7 behaviour
# add the 7 behaviour information
behaviourCodeLoop(PlotBeahviour = F, nBehaviours = 7)

# process the data for modeling
setwd("~/Documents/r_files/Seals/")
source('featureProcessingTime.r')
# 75HZ
featData7_75=featureProcessing(nBehaviours = 7, targetCol = "type_event75")
save(featData7_75, file = "featData7_75v2.RData")
corMat <- cor(featData4_75[, sapply(featData4_75, class) == "numeric"])
max(abs(corMat[upper.tri(corMat)]))

# 25
featData7_25=featureProcessing(nBehaviours = 7, targetCol = "type_event25")
save(featData7_25, file = "featData7_25v2.RData")

# 13
featData7_13=featureProcessing(nBehaviours = 7, targetCol = "type_event13")
save(featData7_13, file = "featData7_13v2.RData")

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
