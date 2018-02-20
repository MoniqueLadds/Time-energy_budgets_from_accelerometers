library(lubridate)

argos<-read.csv("data/argos_data/JuvenileFS_argos_LJP_SR_clean.csv",stringsAsFactors = FALSE)
table(argos$DeployID,argos$Location)

argos$id<-paste0(argos$DeployID,argos$Location)

str(argos)

argos$Date<-as.POSIXct(argos$Date,format="%d/%m/%Y %H:%M")
argos$seconds<-seconds(argos$Date)

time.diff<-aggregate(Date~id,argos,diff.POSIXt)

b<-NULL
for(i in 1:9){
  x<-as.numeric(unlist(time.diff$Date[i]))
  b<-rbind(b,summary(x))
}
b<-cbind(time.diff$id,b)
write.csv(b,"output/summary_intervals.csv")



