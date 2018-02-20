source("R\\functions.R")
library(zoo)
library(pracma)
library(MASS)
library(KernSmooth)
library(diveMove)
library(lubridate)
library(data.table)

####read in data#####
data<-read.csv("data\\Wild\\A09804\\A09804_1hz.csv",header=TRUE,stringsAsFactors=FALSE)
str(data)
data$Date.Time<-as.POSIXct(strptime(data$Date.Time,format="%Y-%m-%d %H:%M:%S"),tz="GMT")
#data$date <- substr(data$Date.Time, 1, 10) 
#data$time <- substr(data$Date.Time, 12, 19) 
#data$date <- as.Date(data$date,format="%d/%m/%Y")

##Filter data
filter<-HampelFilter(data$depth,5,t0=3)
data$smooth.depth<-filter[[1]]
plot(filter[[1]],type="l")

##Create binary variable for dives >5m from pressure####
data$is.dive<-ifelse(data$Pressure>5,1,0)

##Create data set for diving###
diving<-data[which(data$depth>5),]
plot(diving$depth,type="l")
max(diving$depth)

##Create data set for not diving###
not.dive<-data[which(data$depth<=5),]
subb<-not.dive[1:10000,]
plot(subb$depth,type="l")

?system.file
###Create TDR data####
example(diveMove)

fp <- file.path("data", "Wild", "A09804", "A09804_1hz.csv",fsep="//")
sfp <- system.file("data\\Wild\\A09804\\A09804_1hz.csv", package="diveMove")
tdrX<-readTDR("data\\Wild\\A09804\\A09804_1hz.csv",dateCol="d.time",
              depthCol="depth", speed=FALSE, sep=",", na.strings="", 
              as.is=TRUE)
head(tdrX)

tdrXcsv <- read.csv("Data\\Seal rocks 01.07-01.08\\Penny\\subdata.csv", header=TRUE)
ddtt <- strptime(tdrXcsv$Date.Time, format="%Y-%m-%d %H:%M:%S")
time.posixct <- as.POSIXct(ddtt, tz="GMT")
tdrx<-createTDR(time=time.posixct,
                depth=tdrXcsv$Pressure,
                concurrentData=tdrXcsv[,c(1:6)],
                dtime=1,file=srcfn)
?createTDR


###Create TDR data####
fp <- file.path("Data", "Seal rocks 01.07-01.08", "Penny", "subdata.csv",fsep="/")
sfp <- system.file(subdata, package="diveMove")
tdrX <- readTDR("Data\\Seal rocks 01.07-01.08\\Penny\\subdata.csv",dateCol=1,speed=FALSE, sep=",",na.strings="", as.is=TRUE)
