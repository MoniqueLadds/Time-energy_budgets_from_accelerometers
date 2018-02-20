video<-read.csv("data/examples/ronnie_mav_kerfuffle.csv",header=TRUE)
video$ti<-c(1:length(video$time))

timemax<-length(video$x) # number of frames (and observations - so no interpolation needed)
gname<-paste("g",video$ti,".tif", sep="") # holds the names of the picture files

# draw graphs
for (i in 1:timemax) {
  tiff(filename = gname[i],width=500, height = 360)
  plot(video$ti[1],video$x[1],type="l",ylab="g",xlab="time",
       ylim=c(-4,4),xlim = c(0,1650))
  lines(video$ti[1:i],video$x[1:i])
  lines(video$ti[1:i],video$y[1:i],col="darkgreen")
  lines(video$ti[1:i],video$z[1:i],col="orange")
  dev.off(dev.cur())
}

