rm(list=ls())

source("R\\functions.R")
library(zoo)
library(pracma)
library(MASS)
library(KernSmooth)
library(diveMove)
library(lubridate)
library(data.table)
library(plyr)

files<-list.files("data/wildraw/")

for(i in 1:length(files)){

filename <- paste0("data/wildraw/",files[i])
data <- readLines(filename)

blank <- which(data == "")

## Start and end of interesting blocks of data, ignoring the blank rows.
block.start <- blank[diff(c(blank, Inf)) > 1] + 1
## This is a hack.  Think about how to do it more nicely.
block.start <- c(1, block.start[-length(block.start)])
block.end   <- blank[diff(c(0, blank)) > 1] - 1

block.first <- data[block.start]

## These are regular expressions.  See ?regexp
re.data <- "^Data Block ([0-9]+)$"
re.log <- "^Fast Log ([0-9]+)$"

idx.data <- grep(re.data, block.first)
idx.log <- grep(re.log, block.first)

contents.data <- lapply(idx.data, function(i)
  read.data(data.sub(i, block.start, block.end, data)))


acc<-data.frame(contents.data[3])
colnames(acc)<-c("date","x","y","z")
acc$time<-as.POSIXct(acc$date,format="%d/%m/%Y %H:%M:%S")

depth<-data.frame(contents.data[1])
colnames(depth)<-c("date","depth")
depth$time<-as.POSIXct(depth$date,format="%d/%m/%Y %H:%M:%S")

temperature<-data.frame(contents.data[2])
colnames(temperature)<-c("date","temp")
temperature$time<-as.POSIXct(temperature$date,format="%d/%m/%Y %H:%M:%S")



##extract wet dry log?
x<-(data[block.start[length(block.start)]:length(data)])
x<-(x[-1])
x<-(x[-1])
fields = strsplit(x, '     ')

wet<-t(rbindlist(list(lapply(fields, function(l) l[1]))))
dry<-t(rbindlist(list(lapply(fields, function(l) l[2]))))


####making all devices start at midnight of the 8th 
# january as they were all deployed by this time
start<-"08/01/2014 00:00:00.000"

drychange<-wet[1:(nrow(wet)-2)]
wet<-c(start,wet[1:(nrow(wet)-3)])
dry<-c(start,dry[1:(nrow(dry)-3)])

wetdry<-data.frame(cbind(wet,dry,drychange))
colnames(wetdry)<-c("wet","dry","drychange")
wetdry$wet<-as.character(wetdry$wet)
wetdry$dry<-as.character(wetdry$dry)
wetdry$drychange<-as.character(wetdry$drychange)

options(digits = 2,scipen = 999)

wetdry$wet<-as.POSIXct(wetdry$wet,format="%d/%m/%Y %H:%M:%OS")
wetdry$dry<-as.POSIXct(wetdry$dry,format="%d/%m/%Y %H:%M:%OS")
wetdry$drychange<-as.POSIXct(wetdry$drychange,format="%d/%m/%Y %H:%M:%OS")


wetdry$wetdiff<-as.numeric(difftime(wetdry$dry,wetdry$wet,units = "mins"))
wetdry$drydiff<-as.numeric(difftime(wetdry$drychange,wetdry$dry, units = "mins"))

y<-wetdry[wetdry$wetdiff>10,c(1,2,4)]
colnames(y)<-c("start","end","diff")
y$difftype<-"wet"

x<-wetdry[wetdry$drydiff>10,c(2,3,5)]
colnames(x)<-c("start","end","diff")
x$difftype<-"dry"

xy<-rbind(x,y)
row.names(xy)<-as.numeric(gsub("V","",row.names(xy)))
xy<-xy[ order(as.numeric(row.names(xy))), ]

###merge all the files together

acc<-merge(acc,depth[,2:3],by="time",all.x=TRUE)
acc<-merge(acc,temperature[,2:3],by="time",all.x=TRUE)
acc$date<-as.POSIXct(acc$date,format="%d/%m/%y %H:%M:%OS")

z<-apply(acc,1,function(i) ifelse(test = acc$date[i] > xy$wet & acc$date < xy$dry,xy$type[i],""))


save(acc,file=paste0("data/wilddata_1Hz/",strsplit(files[i],"[.]")[[1]][1],".RData"))
     


}     




#Look at a dive
install.packages(ggplot2)
install.packages(gridExtra)
dive<-unique(acc$divenum)
dives<-rle(acc$depth)
dives<-acc[acc$depth>4,]
dives$time<-as.factor(dives$time)
dives$time<-gsub("0014","2014",dives$time)
dives$time<-as.POSIXct(dives$time,format="%Y-%m-%d %H:%M:%S")
#sample<-df[df$divenum==dive[271],]
sample<-acc[125000:130000,]
p1<-ggplot(aes(x=time,y=x),data=sample)+
  geom_line()
p2<-ggplot(aes(x=time,y=-depth),data=sample)+
  geom_line()
grid.arrange(p1,p2)
