rm(list=ls())

files<-list.files("output/outputDirectory/captive/nofeatures/")
files<-files[!files=="desktop.ini"]


xgb<-NULL
boost<-NULL

for (i in 1:length(files)){
load(paste0("output/outputDirectory/captive/nofeatures/",files[i]))
samples<-substr(files[i],12,17)
epochs<-substr(files[i],20,31)


sensitivty<-outputData$confusionMat$byClass[1:4]
specificity<-outputData$confusionMat$byClass[5:8]


results<-cbind(samples,epochs,sensitivty)
results<-data.frame(cbind(results,specificity))
results$behaviour<-c("foraging","grooming","resting","travelling")
xgb<-rbind(xgb,results)

accuracy<-outputData$confusionMat$overall[1]
lower<-outputData$confusionMat$overall[3]
upper<-outputData$confusionMat$overall[4]
kappa.r<-outputData$confusionMat$overall[2]
bestParam<-outputData$bestParams
n<-sum(outputData$confusionMat$table)

output<-data.frame(cbind(samples,epochs,accuracy,lower,upper,kappa.r,n,bestParam))
boost<-rbind(boost,output)
}


write.csv(xgb,"output/summaries/model.output.csv")

write.csv(boost,"output/summaries/model.results.csv")


#######PLOTS################################################

library(ggplot2)

limits<-aes(xmax=results$upper, xmin = results$lower)
limit<-aes(ymax=results$accuracy+results$sd, 
           ymin = results$accuracy-results$sd)

##Figure 1 paper
#export at 800 x 450

pd <- position_dodge(width = 0.9)

ggplot(boost, aes(x=samples,y=accuracy),fill=epochs)+
  geom_bar(aes(y=accuracy,fill=epochs),
           stat = "identity",position = "dodge")+
  geom_errorbar(limit,width=0.25,position=position_dodge(0.9))  +
  geom_point(aes(y=acc,fill=epochs),
             size=2.5,position = pd)+
  geom_point(aes(y=kappa.r,fill=epochs),col="red",
             shape="-",size=5,position = pd)+
  #coord_flip()
  #facet_wrap(~model)+
  theme_bw()+
  ylab("Model")+
  xlab("Accuracy")+
  #scale_fill_manual(values=c("black","darkgrey"))+
  scale_y_continuous(labels = scales::percent,limit=c(0,1.0))+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.position=c(.9,.1),
        legend.title=element_blank(),
        legend.key = element_blank())


output$id<-factor(paste0(output$behaviour,output$model))
output$sensitivty<-as.numeric(as.character(output$sensitivty))
output$specificity<-as.numeric(as.character(output$specificity))

ggplot(data=output[output$features=="4Feat",],
       aes(x=sensitivty,y=behaviour))+
  geom_segment(aes(yend=behaviour),xend=0)+
  geom_point()+
  facet_wrap(~samples+model,scales = "free_y",labeller = label_context)+
  theme_bw()+
  xlab("Model")+
  ylab("Accuracy")+
  #scale_color_manual(values=c("black","darkgrey","red"))+
  scale_x_continuous(labels = scales::percent,limits = c(.6,1))+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.position=c(.9,.1),
        legend.title=element_blank(),
        legend.key = element_blank())

output$behaviour<-as.factor(output$behaviour)
output$samples<-as.factor(output$samples)


ggplot(data=output[output$features=="4Feat",], aes(x=behaviour,y=sensitivty),fill=samples)+
  geom_bar(stat="identity",aes(fill=samples),position="dodge")+
  facet_wrap(~model)+
  theme_bw()+
  xlab("Model")+
  ylab("Accuracy")+
  scale_fill_grey()+
  scale_y_continuous(labels = scales::percent)+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.position=c(1,.1),
        legend.title=element_blank(),
        legend.key = element_blank())


ggplot(data=output[output$features=="6Feat",], aes(x=behaviour,y=sensitivty),fill=samples)+
  geom_bar(stat="identity",aes(fill=samples),position="dodge")+
  facet_wrap(~model)+
  theme_bw()+
  xlab("Model")+
  ylab("Accuracy")+
  scale_fill_grey()+
  scale_y_continuous(labels = scales::percent)+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.position=c(1,.1),
        legend.title=element_blank(),
        legend.key = element_blank())

