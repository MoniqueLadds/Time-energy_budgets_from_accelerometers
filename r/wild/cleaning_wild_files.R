clean_1Hz<-function(x){

setwd("I:/Juvenile fur seal energetics")
library(lubridate)
library(qdapTools)

deets<-read.csv("start.end_times.csv",stringsAsFactors=FALSE)
deets$start.date<-as.POSIXct(deets$start.date,format="%d/%m/%Y %H:%M")
deets$end.date<-as.POSIXct(deets$end.date,format="%d/%m/%Y %H:%M")
deets$start.secs<-seconds(deets$start.date)
deets$end.secs<-seconds(deets$end.date)

files<-list.files("data/wilddata_1Hz/")

i=x

#for(i in 1:length(files)){

#load 1Hz data
load(paste0("data/wilddata_1Hz/",files[i]))    #acc

cat(paste0("data/wilddata_1Hz/",files[i]),"\n")

acc$secs<-seconds(acc$date)


#load 20Hz data
load(paste0("data/wilddata/",files[i]))   #df

df$secs<-seconds(df$date)

###find start time
#use start and end time set manually
seal<-substr(files[i],1,(nchar(files[i])-6))

start.time<-lookup(seal,deets[,c("id","start.secs")])
end.time<-lookup(seal,deets[,c("id","end.secs")])

##clean 1Hz file (remove data before acc was on seal)
acc<-acc[as.numeric(acc$secs)>start.time&as.numeric(acc$secs)<end.time,]

###extract dives from 1Hz data

#find number of dives collected (any fast HZ)
dives<-unique(df$divenum)

testing<-NULL

sample<-df[df$divenum==1,]
start.t<-sample$secs[1]
end.t<-sample$secs[nrow(sample)]
testing<-acc[acc$secs<=start.t|acc$secs>=end.t,]

#Find the start and end time of all fast periods and extract dives from 1HZ
for(m in 2:length(dives)){
  sample<-df[df$divenum==m,]
  #cat(dives[m])
  start.t<-sample$secs[1]
  end.t<-sample$secs[nrow(sample)]
  #test<-acc[acc$secs>=start.t&acc$secs<=end.t,]
  #test$divenum<-m
  #new.df<-rbind(new.df,test)
  testing<-testing[testing$secs<=start.t|testing$secs>=end.t,]
}

save(testing,file=paste0("output/1Hz_dives/",seal,".RData"))
}
