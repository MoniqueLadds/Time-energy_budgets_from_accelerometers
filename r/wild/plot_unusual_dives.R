rm(list=ls())

plot_unusual_dives<-function(){
source("r/functions.R")

library(ggplot2)
#library(multiplot)

seal<-list.files("data/wilddata/")
#i=1

for (i in 1:length(seal)){
load(paste0("data/wilddata/",seal[i]))
  cat(paste0("data/wilddata/",seal[i]))

#summary(df)

#df$hr<-hour(df$time)
#df$mn<-minute(df$time)
#df$sc<-second(df$time)

dive<-unique(df$divenum)
dives<-rle(df$divenum)
dives<-data.frame(unclass(dives))
cat(max(dives$lengths),"/n")
long<-dives[dives$lengths>3600,2]

for(l in 1:length(long)){
sample<-df[df$divenum==long[l],]
cat(paste0(substr(seal[i],1,nchar(seal[i])-6),
       " ","dive no. ",long[l]," ","date: ",
       sample$date[1]),"\n")
#summary(sample)
p1<-ggplot(data=sample,aes(y=x,x=date))+
  geom_line()+
  theme_bw()+
  ggtitle(paste0(substr(seal[i],1,nchar(seal[i])-6),
                 " ","dive no. ",long[l]," ","date: ",
                 sample$date[1]))+
  scale_y_continuous(limits=c(-2,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p2<-ggplot(data=sample,aes(y=-depth,x=date))+
  geom_line()+
  theme_bw()

tiff(file=paste0("figs/unusual_dives/",substr(seal[i],1,nchar(seal[i])-6),
                     "_",long[l],".tiff"))
multiplot(p1,p2)
dev.off()

}

}
}
