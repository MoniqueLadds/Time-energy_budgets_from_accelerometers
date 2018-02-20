rm(list=ls())

source("R/functions.R")

library(zoo)
library(pracma)
library(MASS)
library(KernSmooth)
library(diveMove)
library(data.table)
library(plyr)

files<-list.files("data/wild/")

filename <- paste0("data/wild/",files[1])

###read the data in - takes a while
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

## Get all the data.  Will take about a minute per set, perhaps a bit
## longer for the third one.
contents.data <- lapply(idx.data, function(i)
  read.data(data.sub(i, block.start, block.end, data)))

logs <- 
  lapply(idx.log[1:n], function(i)
    read.data(data.sub(i, block.start, block.end, data)))

##Check read.data works
x <- read.data(data.sub(idx.log[4], block.start, block.end, data))
n<-length(idx.log)

logs1 <- load.logs(filename, data, TRUE)

saveRDS(logs1, filename.logs(filename))

####read in saved list
logs1<-readRDS(filename.logs(filename))

##make into dataframe with dive number
names(logs1) <- seq(1, length(logs1)) 

df <- do.call("rbind.fill",  logs1)
df$divenum <- rep(names(logs1), sapply(logs1, nrow))

names(df)<-c("Date.Time.Stamp","x.axis","y.axis","z.axis","divenum")
df$Date.Time<-as.POSIXct(strptime(df$Date.Time.Stamp,format="%d/%m/%Y %H:%M:%S"),tz="GMT")
op <- options(digits.secs=2)
df$Time.Seconds<-strptime(df$Date.Time.Stamp,format="%d/%m/%Y %H:%M:%OS")

#read in dive depth data
##change for each animal
#chrissy<-read.csv("data/wild/A10284_chrissy.csv",skip=71,nrows=1693620) 
dive<-read.csv("data/wild/A09804_kevin.csv",skip=108,nrows=2678400) 

##Filter data
filter<-hampel(dive$Pressure,5,t0=3)
dive$smooth.depth<-filter[[1]]

##Create data set for diving###
diving<-dive[which(dive$smooth.depth>1.5),]
diving$Date.Time<-as.POSIXct(strptime(diving$Date.Time.Stamp,format="%d/%m/%Y %H:%M:%S"),tz="GMT")

df<-merge(df,diving,by="Date.Time",all.x=TRUE)

save(df,file="kevin.RData")


plot(df[df$divenum==4,]$smooth.depth)
