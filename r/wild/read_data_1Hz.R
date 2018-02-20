rm(list=ls())

source("R\\functions.R")
library(zoo)
library(pracma)
library(MASS)
library(KernSmooth)
library(lubridate)
library(data.table)
library(plyr)

files<-list.files("data/wildraw/")

i=3


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



###merge all the files together

acc<-merge(acc,depth[,2:3],by="time",all.x=TRUE)
acc<-merge(acc,temperature[,2:3],by="time",all.x=TRUE)
acc$date<-as.POSIXct(acc$date,format="%d/%m/%y %H:%M:%OS")


save(acc,file=paste0("data/wilddata_1Hz/",strsplit(files[i],"[.]")[[1]][1],".RData"))
     


}     

