rm(list=ls())

files<-list.files("output/outputDirectory/")
files<-files[!files=="test"]

stf<-NULL
stuff<-NULL

for (i in 1:length(files)){
load(paste0("output/outputDirectory/",files[i]))
samples<-as.numeric(substr(files[i],regexpr("epochs",files[i])+9,regexpr(".RData",files[i])-1))
Hz<-as.numeric(substr(files[i],regexpr("Hz",files[i])+5,regexpr(", e",files[i])-1))

sensitivty<-outputData$confusionMat$byClass[1:4]
specificity<-outputData$confusionMat$byClass[5:8]
n<-sum(outputData$confusionMat$table)

results<-cbind(samples,Hz,sensitivty)
results<-cbind.data.frame(results,specificity)
results$behaviour<-c("foraging","grooming","resting","travelling")
#results$model<-"SL"
stf<-rbind(stf,results)

accuracy<-as.numeric(outputData$confusionMat$overall[1])
lower<-as.numeric(outputData$confusionMat$overall[3])
upper<-as.numeric(outputData$confusionMat$overall[4])
kappa.r<-as.numeric(outputData$confusionMat$overall[2])
#bestParam<-outputData$bestParams
n<-sum(outputData$confusionMat$table)

output<-cbind.data.frame(samples,Hz,accuracy,lower,upper,kappa.r,n)
#output$model<-"SL"
stuff<-rbind(stuff,output)
}


library(ggplot2)

limits<-aes(xmax=stuff$upper, xmin = stuff$lower)
limit<-aes(xmax=stuff$accuracy+stuff$sd, xmin = stuff$accuracy-stuff$sd)
stuff$groups<-as.factor(paste0(stuff$Hz,stuff$samples))

##Figure 1 paper
#export at 650 x 350

ggplot(stuff, aes(x=accuracy,y=groups))+
  geom_point()+
  geom_errorbarh(limits)+
  theme_bw()+
  ylab("Combinations")+
  scale_x_continuous(labels = scales::percent,limit=c(0.5,1))+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16))


###Importance####


conf.mat<-outputData$confusionMat$table

importance<-data.frame(outputData$varImp)

importance$names<-rownames(importance)

## Re-level the variables by importance

#export at 800 x 1000
importance$names <-factor(importance$Feature, 
                          levels=importance[order(importance$Gain), 
                                            "Feature"])

x <-ggplot(importance, aes(y=names, x=Gain)) + 
  geom_point(stat="identity")+
  ylab("Variable")+
  xlab("Mean decrease accuracy")+
  scale_x_continuous(labels = scales::percent,limits=c(0,0.045))+
  theme(panel.grid=element_blank(),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill="NA"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"))

y <-ggplot(importance, aes(x=names, y=Gain)) + 
  geom_bar(stat="identity") + 
  coord_flip()+
  xlab("Summary statistic")+
  ylab("Mean decrease accuracy")+
  theme(panel.grid=element_blank(),
        panel.background=element_blank(),
        panel.border=element_rect(colour="black",fill="NA"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"))

grid.arrange(x, y, ncol=2)

