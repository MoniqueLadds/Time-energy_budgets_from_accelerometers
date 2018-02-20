rm(list=ls())

library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)


source('r/wild/cleaning_wild_files.R')

origin <- ymd_hms("1970-01-01 00:00:00")  

dives<-read.table("data/wildlifecomp/Chrissy/12A0605_5_caught26thJan.tab")
dives$Date<-as.POSIXlt(dives$Time,origin=origin)
dives$times <- origin+(dives$Time+39600) 

#dives<-dives[10000:nrow(dives),]

SealFiles<-list.files("data/wilddata_1Hz/")
load("data/wilddata_1Hz/A10284_chrissy.RData")

acc$time_stamp<-as.integer(seconds(acc$date))


start.time<-acc$date[1]
end.time<-acc$date[nrow(acc)]

df<-dives[dives$times>=start.time&dives$times<=end.time,]

df<-merge(acc,df,by.x = "date",by.y="times")

start<-"2014-01-23 00:30:00"
end<-"2014-01-24 23:30:00"
plotData<-df[df$date>start&df$date<end,]

p1<-ggplot(data = plotData, aes(x=date,y=depth))+
  geom_line()+
  theme_bw()+
  #scale_y_continuous(limits=c(0,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p2<-ggplot(data=plotData, aes(x=date,y=x))+
  geom_line(aes(y=x))+
  geom_line(aes(y=y,colour="green"))+
  geom_line(aes(y=z,colour="blue"))+
  theme_bw()+
  scale_y_continuous(limits=c(-2,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none")
p3<-ggplot(data=plotData,aes(x=date,y=Depth))+
  geom_line()+
  theme_bw()+
  ylab("")+
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())


p1 <- ggplot_gtable(ggplot_build(p1))
p2 <- ggplot_gtable(ggplot_build(p2))
p3 <- ggplot_gtable(ggplot_build(p3))


maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], 
                     p3$widths[2:3])
p1$widths[2:3] <- maxWidth
p2$widths[2:3] <- maxWidth
p3$widths[2:3] <- maxWidth

grid.arrange(p1, p2, p3, ncol = 1)


