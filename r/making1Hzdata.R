rm(list=ls())

##read in the 1Hz data
family<-"fur seal"
place<-"Taronga"
seal<-"mav"
date<-"03-09-14"
files<-paste0("data/behaviour_1Hzraw/",family,"/",place,"/",seal,"/",date,".csv")
#files<-list.files(paste("data","behaviour_1Hzraw",place,seal,file)
#direct<-paste("data","behaviour_1Hzraw",place,seal,"/",sep="/")

df<-read.csv(files,stringsAsFactors = FALSE)
colnames(df)<-c("date","depth","x","y","z")
df$date<-as.POSIXct(df$date,format="%H:%M:%S")
df$time<-as.factor(df$date)

#read in the matching data and clean

dat<-read.csv(paste0("data/behaviour_rawdata/",seal,"/",date,".csv"),
              stringsAsFactors = FALSE)

#dat$time<-as.POSIXct(dat$date,format="%H:%M:%S")
dat$time<-as.POSIXct(dat$time,format="%Y-%m-%d %H:%M:%S")
dat$time<-as.factor(dat$time)

# copy down behavours first
#ind = which(!dat$raw=="")      # get positions of nonmissing values

# repeat the values at these indices
#dat$behaviour <- rep(dat$raw[ind],times = diff(c(ind, length(dat$raw) + 1) ))

#dat<-dat[seq(from=1,to=nrow(dat),by=20),]
nrow(dat)/25/60

output<-merge(df,dat[,c(1:4,11:15,6)],by="time",all.x=TRUE)
output<-output[complete.cases(output),]
output<-output[!duplicated(output),]
nrow(output)/60
              
save(output,file=paste0("data/behaviour_1Hzclean/",seal,"/",date,".RData"))
