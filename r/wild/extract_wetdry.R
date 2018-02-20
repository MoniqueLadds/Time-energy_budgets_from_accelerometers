rm(list=ls())

source("R\\functions.R")
library(zoo)
library(pracma)
library(MASS)
library(KernSmooth)
#library(diveMove)
library(lubridate)
library(data.table)
library(plyr)

files<-list.files("data/wildraw/")

for(i in 2:length(files)){

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

##extract wet dry log?
z<-(data[block.start[length(block.start)]:length(data)])
#x<-(data[block.start[length(block.start)-2]:length(data)])
z<-(z[-1])
z<-(z[-1])
fields = strsplit(z, '     ')

wet<-t(rbindlist(list(lapply(fields, function(l) l[1]))))
dry<-t(rbindlist(list(lapply(fields, function(l) l[2]))))


####choose start time
info<-read.csv("start.end_times.csv",stringsAsFactors = FALSE)
#info$start.date<-as.character(as.POSIXct(info$start.date,format="%d/%m/%Y %H:%M"))
#info$end.date<-as.character(as.POSIXct(info$end.date,format="%d/%m/%Y %H:%M"))
seal.id<-substr(files[i],1,(nchar(files[i])-4))
start<-info[info$id==seal.id,4]

drychange<-wet[1:(nrow(wet)-2)]
wet<-c(start,wet[1:(nrow(wet)-3)])
dry<-c(start,dry[1:(nrow(dry)-3)])

wetdry<-data.frame(cbind(wet,dry,drychange))
colnames(wetdry)<-c("wet","dry","drychange")
wetdry$wet<-as.character(wetdry$wet)
wetdry$dry<-as.character(wetdry$dry)
wetdry$drychange<-as.character(wetdry$drychange)

options(digits.secs = 3,scipen = 999)

wetdry$wet<-as.POSIXct(wetdry$wet,format="%d/%m/%y %H:%M:%OS")
wetdry$dry<-as.POSIXct(wetdry$dry,format="%d/%m/%y %H:%M:%OS")
wetdry$drychange<-as.POSIXct(wetdry$drychange,format="%d/%m/%y %H:%M:%OS")

wetdry<-wetdry[order(wetdry$wet),]

wetdry$wetdiff<-as.numeric(difftime(wetdry$dry,wetdry$wet,units = "mins"))
wetdry$drydiff<-as.numeric(difftime(wetdry$drychange,wetdry$dry, units = "mins"))

y<-wetdry[wetdry$wetdiff>1,c(1,2,4)]
colnames(y)<-c("start","end","diff")
y$difftype<-"wet"

x<-wetdry[wetdry$drydiff>1,c(2,3,5)]
colnames(x)<-c("start","end","diff")
x$difftype<-"dry"

xy<-rbind(x,y)

#row.names(xy)<-as.numeric(gsub("V","",row.names(xy)))
#xy<-xy[ order(as.numeric(row.names(xy))), ]

xy<-xy[order(xy$start),]

seal<-substr(files[i],1,(nchar(files[i])-4))


write.csv(xy,file=paste0("output/wetdry_times/",files[i]),row.names=FALSE)
save(xy,file=paste0("output/wetdry_times/",seal,".RData"))

}

