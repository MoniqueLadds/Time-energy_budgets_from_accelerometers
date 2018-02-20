rm(list = ls())

SealNames <- list.files("data/behaviour_1Hzclean/")

for(j in 1:length(SealNames)){
      files <- list.files(paste("data/behaviour_1Hzclean",SealNames[1],sep="/"))
      
  for(k in 1:length(files)){
    load(paste("data","behaviour_1Hzclean",SealNames[1],files[1],sep="/"))
    
events<-unique(output$behav)

for(i in 1:length(unique(events))){
  tiff(paste("figs","Captive",SealNames[2],paste(events[i],files[1],".tiff",sep=""),sep="/"),width=480)
  plot_data <- output[output$behav==events[i], ]
  plot(x~date, data=plot_data, type="l" ,ylim=c(-6,6),
       xlim=c(min(plot_data$date),max(plot_data$date)),
       ylab="g",xlab="time", main=paste("Plot of ",events[i],files[1],sep=" "))
  lines(y~date, data=plot_data, col = "darkgrey")
  lines(z~date, data=plot_data, col = "orange")
  dev.off()
}


  }
}

