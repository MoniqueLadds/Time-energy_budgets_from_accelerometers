rm(list = ls())

SealNames <- list.files("data/fur seals/behaviour_1Hzclean/")

for(j in 1:length(SealNames)){
      files <- list.files(paste("data/fur seals/behaviour_1Hzclean",SealNames[j],sep="/"))
      files <- files[!grepl("Thumbs.db", files)]
      
  for(k in 1:length(files)){
    load(paste("data","fur seals","behaviour_1Hzclean",SealNames[j],files[k],sep="/"))
    output<-output[!duplicated(output[,2:5]),]
    x<-rle(output$type)
    output$behav<-rep(1:length(x$values),x$lengths)
    output$behav<-paste0(output$behav,".",output$type)
            
events<-unique(output$behav)

for(i in 1:length(unique(events))){
  plot_data <- output[output$behav==events[i], ]
  if(nrow(plot_data)>10){
  tiff(paste("figs","Captive",SealNames[j],paste(events[i],"_",files[k],".tiff",sep=""),sep="/"),width=480)
    plot(x~date, data=plot_data, type="l" ,ylim=c(-2,2),
       xlim=c(min(plot_data$date),max(plot_data$date)),
       ylab="g",xlab="time", main=paste("Plot of ",events[i],files[k],sep=" "))
  lines(y~date, data=plot_data, col = "darkgrey")
  lines(z~date, data=plot_data, col = "orange")
  dev.off()
  }#end if
}#end i
  }#end k
}#end j

