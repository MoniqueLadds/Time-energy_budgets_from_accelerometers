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

i=4

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

## as.integer(sub(re.data, "\\1", block.first[idx.data]))
## as.integer(sub(re.log, "\\1", block.first[idx.log]))

## Get all the data.  Will take about a minute per set, perhaps a bit
## longer for the third one.

contents.data <- lapply(idx.data, function(i)
                        read.data(data.sub(i, block.start, block.end, data)))

##Check read.data works
#x <- read.data(data.sub(idx.log[1], block.start, block.end, data))

##Number of dive events

n<-length(idx.log)

##Collect all logs

logs1 <- load.logs(filename, data, TRUE)
names(logs1) <- seq(1, length(logs1)) 
df <- do.call("rbind.fill",  logs1)
df$divenum <- rep(names(logs1), sapply(logs1, nrow))
colnames(df)<-c("date","x","y","z","divenum")
df$time<-as.POSIXct(df$date,format="%d/%m/%Y %H:%M:%S")

depth<-data.frame(contents.data[1])
colnames(depth)<-c("date","depth")
depth$time<-as.POSIXct(depth$date,format="%d/%m/%Y %H:%M:%S")

temperature<-data.frame(contents.data[2])
colnames(temperature)<-c("date","temp")
temperature$time<-as.POSIXct(temperature$date,format="%d/%m/%Y %H:%M:%S")

df<-merge(df,depth,by="time",all.x=TRUE)
df<-merge(df,temperature,by="time",all.x=TRUE)


save(df, file=paste0("data/wilddata/",substr(files[i],1,(nchar(files[i])-4)),".RData"))


