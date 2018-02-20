rm(list=ls())

load(file = "data/wilddata/A10283_bishop.RData")

summary(df)

#Look at a dive

dive<-unique(df$divenum)
dives<-rle(df$divenum)
dives<-data.frame(unclass(dives))

##explore temperature

temp<-df[complete.cases(df),]

av.temp<-tapply(temp$temp,temp$divenum,max)
av.temp[av.temp>25]

sample<-df[df$divenum==dive[1501],]
#sample<-sample[1:500,]
par(mfrow=c(3,1),mar=c(1,2,1,1))
plot(sample$x,type="l",axes=FALSE)
axis(2)
plot(-sample$depth,type="l",axes=FALSE)
axis(2)
#lines(sample$divenum)
plot(sample$temp,axes=FALSE)
axis(2)


library(diveMove)
