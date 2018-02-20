rm(list=ls())

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(nlme)

##READ IN START/END TIMES
deets<-read.csv("data/start.end_times.csv",stringsAsFactors=FALSE)
deets$start.date<-as.POSIXct(deets$start.date,format="%d/%m/%Y %H:%M")
deets$end.date<-as.POSIXct(deets$end.date,format="%d/%m/%Y %H:%M")
deets$start.secs<-as.numeric(seconds(deets$start.date))
deets$end.secs<-as.numeric(seconds(deets$end.date))

###Figure 1 in paper

load("output/proportions_revised.RData")
proportions<-merge(proportions,deets[,c(2,4:5,8:14)],by.x="Seal",by.y="id")
proportions<-plyr::ddply(proportions,.(Seal),transform,prop=value/sum(value))
proportions$location<-proportions$place
proportions$place<-ifelse(proportions$Var.1=="land","Land","Water")
proportions$id<-paste0(proportions$location,"_",substr(proportions$Seal,1,6))


###chi-square test
sum(proportions$value)
chi.sq<- proportions %>% group_by(season,Var.1,Var.2) %>%
  dplyr::summarise(N = sum(value),
                   prop = N/12341322)
write.csv(chi.sq,"output/summaries/chi_square_table_revised.csv")


prop.summ<- proportions %>% group_by(Seal,Var.1,Var.2) %>%
  dplyr::summarise(Prop = sum(prop))

prosum<- prop.summ %>% group_by(Var.1, Var.2) %>%
  dplyr::summarise(minProp = min(Prop),
            maxProp = max(Prop),
            meanProp = mean(Prop))

prosum<- proportions %>% group_by(Seal,Var.1) %>%
  dplyr::summarise(minProp = min(prop),
                   maxProp = max(prop),
                   meanProp = mean(prop),
                   sumProp = sum(prop))
min(prosum[prosum$Var.1=="land",]$sumProp)
max(prosum[prosum$Var.1=="land",]$sumProp)
min(prosum[prosum$Var.1=="underwater",]$sumProp)
max(prosum[prosum$Var.1=="underwater",]$sumProp)
min(prosum[prosum$Var.1=="surface",]$sumProp)
max(prosum[prosum$Var.1=="surface",]$sumProp)




ggplot(prosum,aes(x=Var.2, y=meanProp, fill=Var.1))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Mean proportion")+
  scale_colour_brewer("Dark2")+
  theme_bw()+
  theme(legend.position = c(.2,.8),
        legend.title = element_blank(),
        panel.grid.major = element_blank())

write.csv(prosum,"output/summaries/place_event_revised.csv")

proptab<-proportions %>% group_by(Seal,place,Var.2) %>%
  dplyr::summarise(Prop = sum(prop))

proptab.sum<- proptab %>% group_by(place) %>%
  dplyr::summarise(minProp = min(Prop),
            maxProp = max(Prop))

write.csv(proportions,"output/summaries/proportions_revised.csv")
 
mms.cor <- ddply(.data=proportions, 
                 .(id), 
                 summarize, 
                 n=paste("Days =", mean(days)))


ggplot(proportions,aes(x=Var.2, y=prop, fill=Var.1))+
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent)+
  xlab("")+
  ylab("Percentage")+
  scale_colour_brewer("Dark2")+
  facet_wrap(~id)+
  theme_bw()+
  geom_text(data=mms.cor, aes(x=4, y=.6, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme(legend.position = c(.1,.92),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figs/Barplots activity budgets.eps", width=10, height = 7)

###########################

load("output/all_data_revised.RData")

##add in seal information
summary(all_data)
#all_data<-all_data[complete.cases(all_data),]
all_data<-merge(all_data[,2:11], deets[,c(2,8:13)], by.x="Seal",by.y="id",all.x = TRUE)
all_data$id<-as.factor(paste0(all_data$place,"_",substr(all_data$Seal,1,6)))

  sealPlot<-all_data %>% group_by(id,days,weight,gender,season) %>%
    dplyr::summarise(totEE = sum(EE),
              totTime = sum(Etime),
              Ndays = max(days))
  
  ##add DEE kg to the data
  sealPlot$totKj<-sealPlot$totEE*5*4.186
  sealPlot$EEkg<-sealPlot$totKj/sealPlot$weight
  sealPlot$BMR<-(sealPlot$totKj/4.186)/ (70*sealPlot$weight^0.75)
  sealPlot$Mj_day<-sealPlot$totKj/1000
    
      hist(sealPlot$totKj)
      #find average overall energy expenditure - numbers in results.
      mean(sealPlot[sealPlot$totTime>1300,]$totKj)/1000
      sd(sealPlot[sealPlot$totTime>1300,]$totKj)/1000
      min(sealPlot[sealPlot$totTime>1300,]$totKj)/1000
      max(sealPlot[sealPlot$totTime>1300,]$totKj)/1000
      
      mean(sealPlot$totEE)
      
      mean(sealPlot$EEkg)/1000
      sd(sealPlot$EEkg)/1000
      min(sealPlot$EEkg)/1000
      max(sealPlot$EEkg)/1000
      mean(sealPlot$BMR)  ##for discussion
      mean(sealPlot$totKj)/mean(sealPlot$weight)  ##compare to Dalton 2014
  
      ###appendix 1 or 2 maybe
      
      write.csv(sealPlot,"output/summaries/DEE_all_seals.csv")
      
###Energy by event

sealData<-all_data %>% group_by(id,days,Event,weight) %>%
  dplyr::summarise(EE_day = sum(EE))
##add DEE kg to the data
sealData$totKj<-sealData$EE_day*5*4.186
sealData$EEkg<-sealData$totKj/sealData$weight
sealData$BMR<-(sealData$totKj/4.186)/ (70*sealData$weight^0.75)
sealData$Mj_day<-sealData$totKj/1000


##Figure 3 in paper
ggplot(aes(x=Event, y=Mj_day),data=sealData)+
  geom_boxplot(alpha=0.9)+
  facet_wrap(~id)+
  theme_bw()+
  scale_y_continuous(name = expression(DEE (Mj ~ d^-1)),limits = c(0,22),
                     position = "right")+
  xlab("")+
  theme(panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "transparent",colour = NA),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())

ggsave("figs/DEE_boxplot.eps", width=10, height = 7, bg="transparent")


ggplot(aes(x=Event, y=Mj_day),data=sealData)+
  geom_boxplot(alpha=0.9)+
  theme_bw()+
  scale_y_continuous(name = expression(DEE (Mj ~ d^-1)),limits = c(0,30))+
  xlab("")+
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "transparent",colour = NA))

sealy<-all_data %>% group_by(days,Event) %>%
  dplyr::summarise(EE_day = mean(EE))

  ###hypothesis testing
  library(nlme)
  library(multcomp)
  source("r/functions.R")
  source("r/rsquared_function.R")

      #variables must be factors to do post-hoc testing.
      sealPlot$sex<-as.factor(sealPlot$gender)
      sealPlot$season<-as.factor(sealPlot$season)

options(scipen = 100) ##for ease of reading

#post-hoc test for sex and season
  ####change the input data before running!
  lme_mod<-lme(totEE~sex+season,random = ~1|id, 
               data=sealPlot[sealPlot$totTime>1200,])
  summary(lme_mod)
  assumptions(lme_mod)
  lme.results(lme_mod)
  
    post.lme.sex<-glht(lme_mod, linfct = mcp(sex="Tukey"))
    summary(post.lme.sex)
    ##numbers of the results
    tapply(sealPlot$totKj,sealPlot$gender,mean)
    tapply(sealPlot$totKj,sealPlot$gender,sd)
    
    post.lme.season<-glht(lme_mod, linfct = mcp(season="Tukey"))
    summary(post.lme.season)
    #numbers of the results
    tapply(sealPlot$totKj,sealPlot$season,mean)
    
    tapply(sealPlot$totKj,sealPlot$season,sd)
    
      
##post-hoc test for behaviour events
      
    sealData$Event<-as.factor(sealData$Event)
    ####change the input data before running!
    lme_event<-lme(totKj~Event,random = ~1|id, 
                 data=sealData)
    summary(lme_event)
    assumptions(lme_event)
    lme.results(lme_event)
    
    post.lme.event<-glht(lme_event, linfct = mcp(Event="Tukey"))
    summary(post.lme.event)
    #numbers of the results
    tapply(sealData$totKj,sealData$Event,mean)
    tapply(sealData$totKj,sealData$Event,sd)
    
    ###data for table 3.
    
    sealSumm<-sealPlot[sealPlot$totTime>1300,] %>% group_by(id,weight) %>%
      dplyr::summarise(meanEE = mean(totKj)/1000,
                sdEE = sd(totKj)/1000,
                minEE = min(totKj)/1000,
                maxEE = max(totKj)/1000,
                meanBMR = mean(BMR))
    
write.csv(sealSumm,"output/summaries/Table3_revised.csv")


###data for table 4.
sealData<-all_data %>% group_by(id,Event,weight,Place,days,season) %>%
  dplyr::summarise(EE_day = sum(EE))
##add DEE kg to the data
sealData$totKj<-sealData$EE_day*5*4.186
sealData$EEkg<-sealData$totKj/sealData$weight
sealData$BMR<-(sealData$totKj/4.186)/ (70*sealData$weight^0.75)
sealData$Mj_day<-sealData$totKj/1000


sealSumm<-sealData %>% group_by(Event,Place) %>%
  dplyr::summarise(meanEE = mean(totKj)/1000,
            sdEE = sd(totKj)/1000,
            minEE = min(totKj)/1000,
            maxEE = max(totKj)/1000,
            meanBMR = mean(BMR))

write.csv(sealSumm,"output/summaries/Table4.2_revised.csv")

###data for table 4.1
sealSumm<-sealData %>% group_by(season,Place,Event) %>%
  dplyr::summarise(meanEE = mean(totKj)/1000,
                   sdEE = sd(totKj)/1000,
                   minEE = min(totKj)/1000,
                   maxEE = max(totKj)/1000,
                   meanBMR = mean(BMR))

write.csv(sealSumm,"output/summaries/Table4.1.csv")

sealSumm<-sealData %>% group_by(Event) %>%
  dplyr::summarise(meanEE = mean(totKj)/1000,
                   sdEE = sd(totKj)/1000,
                   minEE = min(totKj)/1000,
                   maxEE = max(totKj)/1000,
                   meanBMR = mean(BMR))
write.csv(sealSumm,"output/summaries/Table4.2.csv")

sealSumm<-sealData %>% group_by(season,Event) %>%
  dplyr::summarise(meanEE = mean(totKj)/1000,
                   sdEE = sd(totKj)/1000,
                   minEE = min(totKj)/1000,
                   maxEE = max(totKj)/1000,
                   meanBMR = mean(BMR))
write.csv(sealSumm,"output/summaries/Table4.3.csv")

#####Summary data for plotting

load("output/summary_output.RData")

DA<-melt(summary_output,id=c("Seal","days"))
DA<-DA[!DA$variable=="kj_per_day",]

DA$behaviour<-as.factor(as.character(revalue(DA$variable, c("Resting"=1,"Grooming"=2,"Foraging"=3,"Travelling"=4))))

positions <- c("Resting","Grooming","Foraging","Travelling")

p1<-ggplot(DA, aes(x=days, y=value))+
  geom_bar(stat = "identity",aes(fill=variable),width=1)+
  scale_y_continuous(labels = scales::percent)+
  #scale_fill_manual(values = positions)+
  scale_fill_brewer("Dark2")+
  facet_wrap(~Seal,nrow=1,scales="free_x")+
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())

p2<-ggplot(summary_output,aes(x=days,y=kj_per_day))+
  geom_bar(stat= "identity",width=1)+
  theme_bw()+
  facet_wrap(~Seal,nrow=1,scales="free_x")+
  theme(#axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank())

p1 <- ggplot_gtable(ggplot_build(p1))
p2 <- ggplot_gtable(ggplot_build(p2))

maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3])
p1$widths[2:3] <- maxWidth
p2$widths[2:3] <- maxWidth

grid.arrange(p1,p2)







###whole data
source("r/figures_functions.R")

sealFiles<-list.files("data/labelled_wilddata/")

load("data/labelled_wilddata/A09804_kevin.RData")

##########plot some examples
#plotData<-acc
start<-"2013-07-13 06:00:00"
end<-"2013-07-13 10:40:00"
plotData<-acc[acc$date>start&acc$date<end,]

plot_all(plotData)

#ggsave(paste0("figs/Example_behaviour_plot",),device="eps")

ggplot(aes(x=date, y=-DepthSmooth),data=acc)+
  geom_line()

table(acc$Place)
