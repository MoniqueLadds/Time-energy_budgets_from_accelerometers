rm(list=ls())

##read in the 1Hz data
family<-"sea lions"


SealNames<-list.files(paste0("data/",family,"/behaviour_1Hzraw/"))

for(i in 1:length(SealNames)){
  
  FileDate<-list.files(paste0("data/",family,"/behaviour_1Hzraw/",SealNames[i]))

  for(j in 1:length(FileDate)){
    files<-paste0("data/",family,"/","behaviour_1Hzraw/",SealNames[i],"/",FileDate[j])
    
    ##read data file
dat<-read.csv(paste0("data/",family,"/behaviour_20Hz/",SealNames[i],"/",FileDate[j]),
              stringsAsFactors = FALSE)

    cat(paste0("data/",family,"/behaviour_20Hz/",SealNames[i],"/",FileDate[j]),"\n")


    x<-nchar(dat$time[1])
    if(x<20){
    dat$time<-as.POSIXct(dat$date,format="%H:%M:%S")
    dat$time<-substr(as.character(dat$time),12,19)
    dat$date<-as.POSIXct(dat$date,format="%H:%M:%S")
    }else{
    dat$time<-as.POSIXct(dat$date,format="%Y-%m-%d %H:%M:%S")
    dat$time<-substr(as.character(dat$time),12,19)
    dat$date<-as.POSIXct(dat$date,format="%Y-%m-%d %H:%M:%S")
    }
    dat$time<-as.factor(dat$time)
   

output<-dat[seq.int(1,nrow(dat),25),]

save(output,file=paste0("data/",family,"/behaviour_1HzcleanCopy/",SealNames[i],"/",substr(FileDate[j],1,8),".RData"))

  }###end j
}###end i
