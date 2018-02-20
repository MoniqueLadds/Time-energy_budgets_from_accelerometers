rm(list=ls())

library(zoo)
source("r//functions.R")

##read in the 1Hz data
family<-"sea lions"

seal<-list.files(paste0("data/",family,"/","behaviour_rawdata/"))

for (i in 1:length(seal)){
date<-list.files(paste0("data/",family,"/","/behaviour_rawdata/",seal[i]))

for (l in 1:length(date)){
files<-paste0("data/",family,"/","behaviour_rawdata/",seal[i],"/",date[l])

options(digits.secs=3)
df<-read.csv(files,stringsAsFactors = FALSE)
time.len<-(nchar(df$time)[1])
ifelse(time.len<=11,
df$time<-as.POSIXct(df$time,format="%H:%M:%OS"),
  df$time<-as.POSIXct(df$time,format="%Y-%m-%d %H:%M:%OS"))

###make a new time file
start.time<-df$time[1]
end.time<-df$time[nrow(df)]
n<-nrow(df)/25*20

new.time<-zoo(,seq(start.time,end.time,length.out = n))
time<-data.frame(seq.POSIXt(start.time,end.time,length.out = n))
colnames(time)<-"time"
time$id<-1

#for X
old.time.x<-(data.frame(df$x,df$time))
old.time.x$id<-2
colnames(old.time.x)<-c("x","time","id")
Z<-merge(time,old.time.x,by=c("time","id"),all.x=TRUE,all.y=TRUE)
Z$x<-na.approx(Z$x,rule=2)
x<-Z[Z$id==1,]
output<-data.frame(x)

#for y
old.time.y<-data.frame(df$y,df$time)
old.time.y$id<-2
colnames(old.time.y)<-c("y","time","id")
Z<-merge(time,old.time.y,by=c("time","id"),all.x=TRUE,all.y=TRUE)
Z$y<-na.approx(Z$y,rule=2)
y<-Z[Z$id==1,]
output<-merge(output,y,by=c("time","id"))

#for z
old.time.z<-data.frame(df$z,df$time)
old.time.z$id<-2
colnames(old.time.z)<-c("z","time","id")
Z<-merge(time,old.time.z,by=c("time","id"),all.x=TRUE,all.y=TRUE)
Z$z<-na.approx(Z$z,rule=2)
z<-Z[Z$id==1,]
output<-merge(output,z,by=c("time","id"))

#for depth
old.time.depth<-data.frame(df$depth,df$time)
old.time.depth$id<-2
colnames(old.time.depth)<-c("depth","time","id")
Z<-merge(time,old.time.depth,by=c("time","id"),all.x=TRUE,all.y=TRUE)
Z$depth<-na.approx(Z$depth,rule=2)
depth<-Z[Z$id==1,]
output<-merge(output,depth,by=c("time","id"))

#output<-cbind(output,time)
df$id<-2
df<-merge(df,output,all.x=TRUE,all.y=TRUE,by=c("time","id","x","y","z","depth"))

df<-data.frame(sapply(df,replace_na_last,simplify = FALSE))
df<-df[df$id==1,]

write.csv(df,
          paste0("data/",family,"/","/behaviour_20Hz/",
                 seal[i],"/",date[l]),row.names=FALSE)

}}
