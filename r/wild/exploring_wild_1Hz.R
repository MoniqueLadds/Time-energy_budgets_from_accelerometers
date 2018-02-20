rm(list=ls())

source("r/functions.R")

files<-list.files("output/1Hz_dives/")
files2.0<-list.files("data/wilddata/")

i=8

#load 20Hz data
load(paste0("data/wilddata/",files2.0[i]))     #DF
#head(df)
table(df$Place)

#load 1Hz data and label events

load(paste0("output/1Hz_dives/",files[i]))   #TESTING
testing<-testing[,-1]
save(testing,file=paste0("output/1Hz_dives/",files[i]))

###summary of long events
dives<-unique(df$divenum)
events<-unique(testing$event)

max(df$depth)
mean(plotData$temp,na.rm=TRUE)

x<-data.frame(unclass(rle(testing$event)))
x[x$lengths>20000,]


library(ggplot2)

plotData<-testing[testing$event==315,]
a=1
#b=445
b=nrow(plotData)
p1<-ggplot(data=plotData[a:b,],aes(y=x,x=time))+
  geom_line()+
  theme_bw()+
  ggtitle(paste0(substr(files[i],1,nchar(files[i])-5),
                 " ","date: ",
                 plotData$date[1]))+
  scale_y_continuous(limits=c(-2,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p2<-ggplot(data=plotData[a:b,],aes(y=-depth,x=time))+
  geom_line()+
  theme_bw()

multiplot(p1,p2)


diveData<-df[df$divenum==317,]

p1<-ggplot(data=diveData,aes(y=x,x=date))+
  geom_line()+
  theme_bw()+
  ggtitle(paste0(substr(files[i],1,nchar(files[i])-4),
                 " ","dive no. ",1845," ","date: ",
                 diveData$date[1]))+
  scale_y_continuous(limits=c(-2,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p2<-ggplot(data=diveData,aes(y=-depth,x=date))+
  geom_line()+
  theme_bw()

multiplot(p1,p2)



par(mfrow=c(2,1),mar=c(4,3,1,1))
plot(plotData[a:b,]$date,plotData[a:b,]$x,type="l",axes=F)
axis(2)
plot(plotData[a:b,]$date,-plotData[a:b,]$depth,type="l",axes=F)
axis(1)
axis(2)

par(mfrow=c(2,1),mar=c(4,3,1,1))
plot(diveData$date,diveData$x,type="l",axes=F)
plot(diveData$date,-diveData$depth,type="l",axes=F)
axis(1)
axis(2)
