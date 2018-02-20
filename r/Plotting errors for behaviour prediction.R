######################################################################
##summarise machine learning stats##

library(dplyr)
library(ggplot2)


###dont need to run this every time!!!!
options(digits=5,scipen = 100)

outputs<-list.files("output/outputDirectory/wild/1Hz-nofeature/")
Travels<-NULL
Rests<-NULL
Grooms<-NULL
Forages<-NULL
totals<-NULL

for(l in 1:length(outputs)){
  
  load(paste0("output/outputDirectory/wild/1Hz-nofeature/",outputs[l]))
  
  testData<-outputData$testDataSplit
  
  EventProbs<-testData$prob
  EventProbs$Event <- colnames(EventProbs)[max.col(EventProbs, ties.method = 'first')]
  EventProbs$EventValue <- apply(EventProbs[,1:4],1,max)
  
  Travel<-EventProbs[EventProbs$Event=="Travelling",]
  Travel$DiffEvent <- colnames(Travel)[max.col(Travel[,1:3], ties.method = 'first')]
  Travel$DiffEventValue <-   apply(Travel[,1:3],1,max)
  Travel$Diff<-Travel$EventValue-Travel$DiffEventValue
  
  Travels<-rbind(Travels,Travel)
  
  Forage<-EventProbs[EventProbs$Event=="Foraging",]
  Forage$DiffEvent <- colnames(Forage[,2:4])[max.col(Forage[,2:4], ties.method = 'first')]
  Forage$DiffEventValue <-   apply(Forage[,2:4],1,max)
  Forage$Diff<-Forage$EventValue-Forage$DiffEventValue
  
  Forages<-rbind(Forages,Forage)
  
  Groom<-EventProbs[EventProbs$Event=="Grooming",]
  Groom$DiffEvent <- colnames(Groom[,c(1,3:4)])[max.col(Groom[,c(1,3:4)], ties.method = 'first')]
  Groom$DiffEventValue <-   apply(Groom[,c(1,3:4)],1,max)
  Groom$Diff<-Groom$EventValue-Groom$DiffEventValue
  
  Grooms<-rbind(Grooms,Groom)
  
  Rest<-EventProbs[EventProbs$Event=="Resting",]
  Rest$DiffEvent <- colnames(Rest[,c(1:2,4)])[max.col(Rest[,c(1:2,4)], ties.method = 'first')]
  Rest$DiffEventValue <-   apply(Rest[,c(1:2,4)],1,max)
  Rest$Diff<-Rest$EventValue-Rest$DiffEventValue
  
  Rests<-rbind(Rests, Rest)
  
  
  total<-nrow(EventProbs)
  totals<-rbind(totals,total)
}

Groom<-Grooms %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Grooms))
Groom$Var<-'Groom'

Travel<-Travels %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Travels))
Travel$Var<-'Travel'
Forage<-Forages %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Forages))
Forage$Var<-'Forage'
Rest<-Rests %>% group_by(DiffEvent) %>%
  summarise(Diffmean = mean(DiffEventValue),
            DiffSD = sd(DiffEventValue),
            Diffmin = min(DiffEventValue),
            Diffmax = max(DiffEventValue),
            DiffN = n(),
            DiffProp = DiffN/nrow(Rests))
Rest$Var<-'Rest'

ConfMat<-rbind(Groom, Rest, Travel, Forage)
#write.csv(ConfMat, "output/summaries/Wild_confusion_matrix.csv")

(sum(totals)-sum(nrow(Groom),nrow(Rest),nrow(Travel),nrow(Forage)))/sum(totals)


library(ggplot2)
library(reshape)
library(gridExtra)

##Foraging events
Forages$events <- rank(x = Forages$Foraging) 
forage.m <- melt(Forages[,c(1:4,10)],id.vars = "events")

#ggplot(forage.m, aes(x=events, y = value, fill = variable))+
#geom_bar(stat = "identity",width = 1)

hist(Forages$Foraging, breaks = 50)

forage.dens <- ggplot(forage.m, aes(value, fill = variable))+
  geom_density(aes(y=..scaled..),alpha = 0.2)+
  theme_bw()+
  ggtitle("Foraging")+
  ylab("Scaled density")+
  xlab("")+
  scale_x_continuous(limits = c(0,1), expand = c(0, 0), breaks = c(0,0.2,0.4,0.6,0.8,1),
                     labels = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(labels = scales::percent,limit=c(0,1.0), expand = c(0, 0))+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=12,color="black"),
        axis.title.y=element_text(size=12,color="black"),
        axis.text.x=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        legend.position="none",
        legend.title=element_blank(),
        legend.key = element_blank())

##Travelling events
Travels$events <- rank(x = Travels$Travelling) 
travel.m <- melt(Travels[,c(1:4,10)],id.vars = "events")

#ggplot(travel.m, aes(x=events, y = value, fill = variable))+
 # geom_bar(stat = "identity",width = 1)

hist(Forages$Foraging, breaks = 50)

travel.dens <- ggplot(travel.m, aes(value, fill = variable))+
  geom_density(aes(y=..scaled..),alpha = 0.2)+
  theme_bw()+
  ggtitle("Travelling")+
  ylab("")+
  xlab("")+
  scale_x_continuous(limits = c(0,1), expand = c(0, 0), breaks = c(0,0.2,0.4,0.6,0.8,1),
                     labels = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(labels = scales::percent,limit=c(0,1.0), expand = c(0, 0))+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=12,color="black"),
        axis.title.y=element_text(size=12,color="black"),
        axis.text.x=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        legend.position="none",
        legend.title=element_blank(),
        legend.key = element_blank())


##Grooming events
Grooms$events <- rank(x = Grooms$Grooming) 
groom.m <- melt(Grooms[,c(1:4,10)],id.vars = "events")

#ggplot(groom.m, aes(x=events, y = value, fill = variable))+
 # geom_bar(stat = "identity",width = 1)

hist(Grooms$Grooming, breaks = 50)

groom.dens <- ggplot(groom.m, aes(value, fill = variable))+
  geom_density(aes(y=..scaled..),alpha = 0.2)+
  theme_bw()+
  ggtitle("Grooming")+
  ylab("Scaled density")+
  xlab("Probability of being behaviour")+
  scale_x_continuous(limits = c(0,1), expand = c(0, 0), breaks = c(0,0.2,0.4,0.6,0.8,1),
                     labels = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(labels = scales::percent,limit=c(0,1.0), expand = c(0, 0))+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=12,color="black"),
        axis.title.y=element_text(size=12,color="black"),
        axis.text.x=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        legend.position="none",
        legend.title=element_blank(),
        legend.key = element_blank())



##Resting events
Rests$events <- rank(x = Rests$Resting) 
rest.m <- melt(Rests[,c(1:4,10)],id.vars = "events")

#ggplot(rest.m, aes(x=events, y = value, fill = variable))+
 # geom_bar(stat = "identity",width = 1)

hist(Forages$Foraging, breaks = 50)

rest.dens <- ggplot(rest.m, aes(value, fill = variable))+
  geom_density(aes(y=..scaled..),alpha = 0.2)+
  theme_bw()+
  ggtitle("Resting")+
  ylab("")+
  xlab("Probability of being behaviour")+
  scale_x_continuous(limits = c(0,1), expand = c(0, 0), breaks = c(0,0.2,0.4,0.6,0.8,1),
                     labels = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(labels = scales::percent,limit=c(0,1.0), expand = c(0, 0))+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=12,color="black"),
        axis.title.y=element_text(size=12,color="black"),
        axis.text.x=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        legend.position=c(0.5,0.5),
        legend.title=element_blank())

density.plots <- grid.arrange(forage.dens, travel.dens, groom.dens, rest.dens)

ggsave(filename = "figs/Density_plots.png", width = 16, height = 14, units = "cm",
       density.plots)

