
plot_all<-function(data){
p1<-ggplot(data = plotData, aes(x=date,y=VeDBA))+
  geom_line()+
  theme_bw()+
  scale_y_continuous(limits=c(0,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p2<-ggplot(data=plotData, aes(x=date,y=x))+
  geom_line(aes(y=x))+
  geom_line(aes(y=y,colour="green"))+
  geom_line(aes(y=z,colour="blue"))+
  theme_bw()+
  ylab("Raw \n acceleration")+
  scale_y_continuous(limits=c(-2,2))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none")
p3<-ggplot(data=plotData,aes(x=date,y=Place))+
  geom_point(size=0.5)+
  theme_bw()+
  ylab("")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p3.1<-ggplot(data=plotData,aes(x=date,y=finalPreds))+
  geom_point(size=0.5)+
  theme_bw()+
  ylab("")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())
p4<-ggplot(data=plotData,aes(x=date,y=-DepthSmooth))+
  geom_line()+
  theme_bw()+
  ylab("Depth")+
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank())


p1 <- ggplot_gtable(ggplot_build(p1))
p2 <- ggplot_gtable(ggplot_build(p2))
p3 <- ggplot_gtable(ggplot_build(p3))
p3.1 <- ggplot_gtable(ggplot_build(p3.1))
p4 <- ggplot_gtable(ggplot_build(p4))

maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3], 
                     p3$widths[2:3], p4$widths[2:3],
                     p3.1$widths[2:3])
p1$widths[2:3] <- maxWidth
p2$widths[2:3] <- maxWidth
p3$widths[2:3] <- maxWidth
p3.1$widths[2:3] <- maxWidth
p4$widths[2:3] <- maxWidth

grid.arrange(p1, p2, p3, p3.1, p4, ncol = 1)}


colour.by.category <- function(x, table) {
  unname(table[x])
}

##Visual inspection##
plot.all<-function(data,name){
  c <-running.mean(data$o2_v_1,150)
  d <- c[seq(1, length(c), 150)]
  x <-running.mean(data$vo2,150)
  y <- x[seq(1, length(x), 150)]
  q <-running.mean(data$wv,150)
  w <- q[seq(1, length(q), 150)]
  a <-running.mean(data$bp,150)
  e <- a[seq(1, length(a), 150)]
  f <-running.mean(data$temp,150)
  g <- f[seq(1, length(f), 150)]
  h <-running.mean(data$fr_c,150)
  j <- h[seq(1, length(h), 150)]
  par(mfrow=c(2,3))
  par(mar=c(2,2,2,2))
  plot(d,xlab="",ylab="Fraction",type="l",
       axes=FALSE,main="Oxygen consumption")
  axis(2)
  plot(y,xlab="",ylab="",type="l",
       axes=FALSE,main="VO2 rate (ml/min)")
  axis(2)
  plot(q,xlab="",ylab="WVP kPa",type="l",
       axes=FALSE,main="Water vapour pressure")
  axis(2)
  plot(j,xlab="Time",type="l",
       axes=FALSE,ylab="FR ml/min",main="Subsample flowrate")
  axis(1)
  axis(2)
  plot(e,xlab="Time",type="l",
       axes=FALSE,ylab="BP kPa",main="Barometric pressure")
  axis(1)
  axis(2)
  plot(f,xlab="Time",type="l",
       axes=FALSE,ylab="Degrees celcius",main="Temperature")
  axis(1)
  axis(2)
  dev.copy(pdf,name)
  dev.off()
  dev.off()
}


#Vo2
plot.vo2<-function(data1,data2,name){
  par(mfrow=c(1,1))
  plot(data2$X3.vo2,xlab="Time (sec)",ylab="Vo2 (l/O2/min)",type="l",
       ylim=c(0,max(data2$X3.vo2)))
  lines(data2$X3.vo2,col="grey")
  lines(data1$X2.vo2,col="black")
  legend("top",c("Before swim","After swim"),
         text.col=c("black","grey"),bty="n",lty=8)
  dev.copy(pdf,name)
  dev.off()
  dev.off()
}



plot.data.by.min<-function(data,cols=2:3,...){
  plot(vo2kg~minute,data,col=cols[Marker],...)
  points(vo2kg~minute,data,col=cols[Marker],type="o",pch=19)
}

plot.data.by.sec<-function(data,data.by.sec,cols=1:n,...){
  plot(vo2~min.sec,data,col=cols[mk],...)
  points(vo2~min.sec,data.by.sec,col=cols[mk],type="o",pch=21)
}


resid.plot<-function(model,data){
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
  plot(model, which = c(1), col = 1, add.smooth = FALSE,
     caption = "")
  plot(data$Age, resid(model), xlab = "Age",
     ylab = "Residuals")
  plot(data$Gender, resid(model), xlab = "Gender",
     ylab = "Residuals")
  par(op)
}

lme.plt<-function(model,data){
  M1 <- fitted(model, level=0)
  J<- order(data$Age); Ages<- sort(data$Age)
  plot(Ages, M1[J], lwd = 2, type="l",lty=3, axes = FALSE, cex.lab = 1,
      ylim = c(0,12), xlim = c(0,25),xaxs = "i",yaxs="i",
      ylab="Average Oxygen consumption (ml/min/kg)", xlab="Age")
}


assumptions<-function(model){
  modresid<-resid(model)
  modpredict<-predict(model)
  modstres<-(modresid-mean(modresid)/sd(modresid))
  modstpre<-(modpredict-mean(modpredict)/sd(modpredict))
  par(mfrow=c(2,2))
  qqnorm(modstres)
  qqline(resid(model))
  acf(resid(model))
  plot(modstpre, modstres, main = "Standardized Residuals Plot", 
       xlab = "Standardized Predicted Values", ylab = "Standardized Residuals")
  abline(0,0)
  hist(modstres, freq = FALSE,
       main="Histogram of Standardised Residuals")
  curve(dnorm(x, mean = mean(modstres), sd = sd(modstres)),
        add=TRUE, col = "red")
}