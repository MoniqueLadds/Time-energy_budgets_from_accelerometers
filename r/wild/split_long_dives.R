split_data<-function(df){

df$depth_int<-as.integer(df$depth)
df$depth_int<-ifelse(df$depth_int<=6,0,1)

df$events<-c(0,cumsum(df$depth_int[-1L] 
                        != df$depth_int[-length(df$depth_int)]))

df$eventNum <- paste0(df$divenum,"_",df$events)

x<-rle(df$eventNum)
df$depth_int<-factor(df$depth_int,labels=c("surface","dive"))

Events<-unique(df$eventNum)

dba<-NULL

for(i in 1:length(Events)){

  dat<-df[df$eventNum==Events[i],]
#Calculate ODBA and VeDBA of the raw data
  dat$xma<-runmean(dat$x,10)       #the running mean is based on the best model
  dat$yma<-runmean(dat$y,10)       #from the energetics paper
  dat$zma<-runmean(dat$z,10)  

  dba<-rbind(dba,dat)
}

dba$dx<-dba$x-dba$xma
dba$dy<-dba$y-dba$yma
dba$dz<-dba$z-dba$zma 

dba$ODBA <- (abs(dba$dx)+abs(dba$dy)+abs(dba$dz))
dba$VeDBA <- sqrt((dba$dx)^2+(dba$dy)^2+(dba$dz)^2)


library(ggplot2)
source("r/functions.R")

plotData<-acc[acc$eventNum==7,]

a=1
#b=445
b=nrow(plotData)
p1<-ggplot(data=plotData[a:b,],aes(y=x,x=date))+
  geom_line()+
  theme_bw()+
  #ggtitle(paste0(substr(files[i],1,nchar(files[i])-5),
   #              " ","date: ",
    #             plotData$date[1]))+
  scale_y_continuous(limits=c(-2,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p2<-ggplot(data=plotData[a:b,],aes(y=-depth,x=date))+
  geom_line()+
  theme_bw()

multiplot(p1,p2)
