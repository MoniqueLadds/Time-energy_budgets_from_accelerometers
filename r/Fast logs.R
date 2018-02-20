source("R\\functions.R")
library(zoo)
library(pracma)
library(MASS)
library(KernSmooth)
library(diveMove)

#####Read in data#######
dat<-read.csv("data/wild/A10284_chrissy.CSV",skip=71)  #Change name!!!!
save(dat,file="data/wild/A10284_chrissy.RData")
load("data/wild/A10284_chrissy.RData")
blanks <- which(dat == "")
names(dat)<-c("c1","c2","c3","c4","c5","c6","c7")
x<-dat[match('Fast Log 1',dat$c1):match('Fast Log 2',dat$c1),1:2]
y<-x[9:nrow(x)-1,]
x

#length(grep("Fast Log",data))
#c1     c2
#5446151                        Fast Log 1       
#5446152 Start Time :- 01/07/13 00:00:00 0       
#5446153                    Fast rate 0.05       
#5446154                   Resolution = 12       
#5446155         Data points available = 4       
#5446156                   Date/Time Stamp X Axis
#5446157             01/07/13 00:00:00.000 -0.168
#5446158             01/07/13 00:00:00.050 -0.172
#5446159             01/07/13 00:00:00.100 -0.164
#5446160             01/07/13 00:00:00.150 -0.156
#5446161                        Fast Log 2       

x <-  read.data(data.sub(idx.log[100], block.start, block.end, data))

###Pull out one dive
logs1[[3214]]

###length of each dive###
lapply(logs1,nrow)
?table

ten<-logs1[[535]]
names(ten)<-c("date.time","x","y","z")
?names
?mapply
?do.call
##Make a data frame
d<-c(logs1)
dives<-rbindlist(d)
names(dives)<-c("d.time","x","y","z")
dives$Date.Time<-as.POSIXct(strptime(dives$d.time,format="%d/%m/%y %H:%M:%S"),tz="GMT")

###Add pressure
?merge
merge(dives,data,by="Date.Time",all.x=TRUE)

str(dives)
plot(sub$X2,type="l")
?plot
?rbindlist
head(dives)

#do.call(rbind.data.frame,d)

head(dives)