library(lattice)

list.files("calibration")
A09864<-read.csv("calibration/A09864_19-06-14.CSV",skip=64,header=TRUE)
A09864<-A09864[,1:6]
A09864$ID<-"A09864"
A09864$Time<-strptime(A09864$Date.Time.Stamp,"%d/%m/%Y %H:%M")
head(A09864)
A10283<-read.csv("calibration/A10283_19-06-14.CSV",skip=57,header=TRUE,
                 stringsAsFactors=FALSE)
A10283<-A10283[,1:6]
A10283$ID<-"A10283"
A10283$Time<-strptime(A10283$Date.Time.Stamp,"%d/%m/%Y %H:%M")
head(A10283)
A10282<-read.csv("calibration/A10282_19-06-14.CSV",skip=54,header=TRUE,
                 stringsAsFactors=FALSE)
A10282<-A10282[,1:6]
A10282$ID<-"A10282"
A10282$Time<-strptime(A10282$Date.Time.Stamp,"%d/%m/%Y %H:%M")
head(A10282)
A10284<-read.csv("calibration/A10284_19-06-14.CSV",skip=53,header=TRUE,
                 stringsAsFactors=FALSE)
A10284<-A10284[,1:6]
A10284$ID<-"A10284"
A10284$Time<-strptime(A10284$Date.Time.Stamp,"%d/%m/%Y %H:%M")
head(A10284)

data<-merge(A09864,A10282,all=TRUE)
data<-merge(data,A10283,all=TRUE)
data<-merge(data,A10284,all=TRUE)
data<-data[13:972,]
str(data)

xyplot(data$Time~data$Pressure,type=c('l','p'),
       groups=data$ID,auto.key=T)

tapply(data$Pressure,data$ID,max)

diff<-aov(Pressure~as.factor(ID),data)
summary(diff)
TukeyHSD(diff)

boxplot(Pressure~ID,data)
