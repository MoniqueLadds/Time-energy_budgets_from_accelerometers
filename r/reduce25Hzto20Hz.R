rm(list=ls())

library(zoo)
source("r//functions.R")

##read in the 1Hz data
family<-"fur seals"

seal<-list.files("data/fur seals/behaviour_rawdata/")

for (i in 1:length(seal)){
date<-list.files(paste0("data/fur seals/behaviour_rawdata/",seal[i]))

for (l in 1:length(date)){
files<-paste0("data/",family,"/","behaviour_rawdata/",seal[i],"/",date[l])

options(digits.secs=3)
df<-read.csv(files,stringsAsFactors = FALSE)
df$time<-as.POSIXct(df$time,format="%Y-%m-%d %H:%M:%OS")


###make a new time file
start.time<-df$time[1]
end.time<-df$time[nrow(df)]
n<-nrow(df)/25*20

new.time<-zoo(,seq(start.time,end.time,length.out = n))
time<-data.frame(seq.POSIXt(start.time,end.time,length.out = n))
colnames(time)<-"time"

#for X
old.time.x<-zoo(df$x,df$time)
Z<-merge(new.time,old.time.x)
Z<-na.approx(Z,rule=2)
x<-Z[index(new.time),]
output<-data.frame(x)

#for y
old.time.y<-zoo(df$y,df$time)
Z<-merge(new.time,old.time.y)
Z<-na.approx(Z,rule=2)
y<-Z[index(new.time),]
output$y<-as.numeric(y)

#for z
old.time.z<-zoo(df$z,df$time)
Z<-merge(new.time,old.time.z)
Z<-na.approx(Z,rule=2)
z<-Z[index(new.time),]
output$z<-as.numeric(z)

#for depth
old.time.depth<-zoo(df$depth,df$time)
Z<-merge(new.time,old.time.depth)
Z<-na.approx(Z,rule=2)
depth<-Z[index(new.time),]
output$depth<-as.numeric(depth)

output<-cbind(output,time)

df<-merge(df,output,all.x=TRUE,all.y=TRUE)

df<-data.frame(sapply(df,replace_na_last,simplify = FALSE))
df<-df[index(time),]

write.csv(df,
          paste0("data/fur seals/behaviour_20Hz/",
                 seal[i],"/",date[l]),row.names=FALSE)

}}
