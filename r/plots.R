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
##Filter data
filter<-HampelFilter(data$depth,5,t0=3)
data$smooth.depth<-filter[[1]]

##Select every 5th row of the data####
sub<- data[seq(1, nrow(data), by=10), ] 

is.data.norm(howard)
dev.copy(pdf,"figs\\A09869_depthhist.pdf")
dev.off()

xyzd.plot<-function(data){
par(mfrow=c(4,1))
par(mar=c(1.5,2,.5,.25))
plot(data$x,type="l",col="blue",axes=FALSE)
axis(2,pos=0)
mtext("x",side=2,line=0,at=0)
plot(data$y,type="l",col="orange",
     axes=FALSE,yaxp=c(-4,4,5))
axis(2,pos=0)
mtext("y",side=2,line=0,at=0)
plot(data$z,type="l",col="dark green",
     axes=FALSE,yaxp=c(-4,4,5))
axis(2,pos=0)
mtext("z",side=2,line=0,at=0)
plot(data$Date.Time,sub$depth,type="l",
     lwd=0.5,axes=FALSE,xlab="",ylab="Depth")
axis(1,pos=10,tick=FALSE,at=c(1373000000,1373500000,1374000000,
            1374500000,1375000000),
     labels=c("Jul 06","Jul 11","Jul 17","Jul 22","Jul 28"))
axis(2,pos=1372625000)
mtext("Depth",side=2,line=0.5,at=40)
dev.copy(pdf,"figs\\A09804_xyzd.pdf")
dev.off()
}

####Smoothing data#######
subdata$depth.kde<-bkfe(subdata$Pressure,drv=2,bandwidth=0.25)

summary(subdata$depth.kde)

##Moving average over 3sec####
subdata$madepth<-ma(subdata$Pressure,3)
subdata$ma.x<-ma(subdata$x,30)
subdata$ma.y<-ma(subdata$y,30)
subdata$ma.z<-ma(subdata$z,30)



###Clean Pressure#######
subdata$Depth<-tsoutliers(subdata$Pressure,plot=FALSE)
hist(subdata$Pressure)
plot(subdata$Pressure,pch=NA,lty=1,type="l")

###Two sample proportions test####
table(prac$dive.d, prac$dive.p,)
prop.test(table(prac$dive.d, prac$dive.p),correct=FALSE)
chisq.test(table(prac$dive.d, prac$dive.p))

###Checking data####
diving<-data[which(data$Pressure>5),]
other<-data[which(data$Pressure<5),]
plot(diving$Pressure,pch=NA,lty=1,type="l")
plot(other$Pressure,pch=NA,lty=1,type="l")

diving$rm.outlier<-outlierMAD(diving$Pressure,k=3)

###Dont know####
plot(diving$Depth,type="l")
diving$diff<-abs(diving$Pressure-diving$Depth)
plot(diving$diff,type="l",axes=FALSE)
plot(diving$Pressure,diving$Depth)
summary(diving$diff)
a<-diving[1:20,]
a$diff
head(diving)
penny<-unique(data)
penny<-subset(data,!duplicated(data$Date.Time))
head(penny)
summary(penny$Depth)





###Identify outliers####
outlier<-tsoutliers(subdata$Pressure,plot=FALSE)

par(mfrow=c(1,1))
plot(outlier,pch=NA,lty=1,type="l")
lines(subdata$Pressure,col="green")
subdata$diff<-subdata$Pressure-suboutlier
summary(subdata$diff)
plot(subdata$diff)