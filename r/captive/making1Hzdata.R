rm(list=ls())

##read in the 1Hz data
family<-"fur seals"


SealNames<-list.files(paste0("data/",family,"/behaviour_1Hzraw/"))

for(i in 1:length(SealNames)){
  
  FileDate<-list.files(paste0("data/",family,"/behaviour_1Hzraw/",SealNames[i]))

  for(j in 1:length(FileDate)){
    files<-paste0("data/",family,"/","behaviour_1Hzraw/",SealNames[i],"/",FileDate[j])
    
    ##read data file
    df<-read.csv(files,stringsAsFactors = FALSE)
    cat(files)
    
    colnames(df)<-c("date","depth","x","y","z")
    df$date<-as.POSIXct(df$date,format="%H:%M:%S")
    df$time<-substr(as.character(df$date),12,19)
    df$time<-as.factor(df$time)

#read in the matching data and clean

dat<-read.csv(paste0("data/",family,"/behaviour_20Hz/",SealNames[i],"/",FileDate[j]),
              stringsAsFactors = FALSE)

    cat(" ",nrow(dat)/20/60,"   ")


    x<-nchar(dat$time[1])
    if(x<20){
    dat$time<-as.POSIXct(dat$date,format="%H:%M:%S")
    dat$time<-substr(as.character(dat$time),12,19)
    }else{
    dat$time<-as.POSIXct(dat$date,format="%Y-%m-%d %H:%M:%S")
    dat$time<-substr(as.character(dat$time),12,19)
    }
    dat$time<-as.factor(dat$time)

# copy down behavours first
#ind = which(!dat$raw=="")      # get positions of nonmissing values

# repeat the values at these indices
#dat$behaviour <- rep(dat$raw[ind],times = diff(c(ind, length(dat$raw) + 1) ))

#dat<-dat[seq(from=1,to=nrow(dat),by=20),]

output<-merge(subset(df,select=-c(depth)),dat[,c("time","depth","behaviour","type","location","place")],by="time",all.x=TRUE)
output<-output[complete.cases(output),]
output<-output[!duplicated(output),]
cat(nrow(output)/60,"\n")
              
save(output,file=paste0("data/",family,"/behaviour_1Hzclean/",SealNames[i],"/",substr(FileDate[j],1,8),".RData"))

  }###end j
}###end i
