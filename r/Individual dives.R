###Read in data####
###Howard - A09869####################################
howard<-read.csv("data\\Wild\\A09869\\A09869_30hz.csv",
                 header=TRUE,stringsAsFactors=FALSE)
howard$Date.Time<-as.POSIXct(strptime(howard$Date.Time,
                                      format="%Y-%m-%d %H:%M:%S"),
                                      tz="GMT")
howard<-howard[,c(1,3,4,5,6,8,9)]
##Filter data
filter<-HampelFilter(howard$depth,5,t0=3)
howard$smooth.depth<-filter[[1]]
names(howard)<-c("Date.Time","x","y","z","diveID","depth",
                 "temp","smooth.depth")
is.data.norm(howard)
hist(howard$x)
dev.copy(pdf,"figs\\A09869_depthhist.pdf")
dev.off()

###Penny - A09867####################################
penny<-read.csv("data\\Wild\\A09867\\A09867_30hz.csv",header=TRUE,stringsAsFactors=FALSE)
penny$Date.Time<-as.POSIXct(strptime(penny$Date.Time,format="%Y-%m-%d %H:%M:%S"),tz="GMT")
penny<-penny[,c(1,3,4,5,6,8,9)]
##Filter data
filter<-HampelFilter(penny$depth,5,t0=3)
penny$smooth.depth<-filter[[1]]
names(penny)<-c("Date.Time","x","y","z","diveID","depth",
                 "temp","smooth.depth")
is.data.norm(penny)
dev.copy(pdf,"figs\\A09867_depthhist.pdf")
dev.off()

###Kevin - A09804####################################
kevin<-read.csv("data\\Wild\\A09804\\A09804_30hz.csv",header=TRUE,stringsAsFactors=FALSE)
kevin$Date.Time<-as.POSIXct(strptime(kevin$Date.Time,format="%Y-%m-%d %H:%M:%S"),tz="GMT")
kevin<-kevin[,c(1,3,4,5,6,8,9)]
##Filter data
filter<-HampelFilter(kevin$depth,5,t0=3)
kevin$smooth.depth<-filter[[1]]
names(kevin)<-c("Date.Time","x","y","z","diveID","depth",
                "temp","smooth.depth")
is.data.norm(kevin)
dev.copy(pdf,"figs\\A09804_depthhist.pdf")
dev.off()

###Analyse bouts###

bout<-howard[howard$diveID==43,]

xyzd.plot(bout)
dev.copy(pdf,"figs\\A09869_bout2.pdf")
dev.off()

plot(bout$Date.Time,bout$depth,type="l",
     ylim=rev(range(bout$depth)),xlab="Time (min)",ylab="Depth")
dev.copy(pdf,"figs\\A09804_bout1.pdf")



#