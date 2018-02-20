rm(list=ls())

source("r/functions.R")
library(reshape)


###get summary energy expenditure values from Ladds et al. 2016 (JCP)
amrData <- read.csv("data/cot_data.csv")
##only keep swims longer than 40 seconds
amrData <- amrData[amrData$time_swum >= 40,]

###only keep trials with recov > 90
amrData <- amrData[amrData$timediff > 90,]

##suggested equation from Andreas
amrData$amr <- amrData$vo2 - (amrData$rmr * ((amrData$time_swum / 60) + (amrData$timediff / 60)))
mean(amrData[amrData$name=="Ronnie",]$amr)
active <- mean(amrData[amrData$name=="Ronnie",]$vo2)


###get summary energy expenditure values from Ladds et al. 2017 (ConPhys)
restData <- read.csv("data/rest_summ.csv")

plot(restData[restData$id=="Ronnie",]$water_temp, restData[restData$id=="Ronnie",]$vo2_l)
resting.lm <- lm(restData[restData$id=="Ronnie",]$vo2_l ~ restData[restData$id=="Ronnie",]$water_temp )
lm.results(ronnie.lm)
rest.winter <- mean(restData[restData$id=="Ronnie"&restData$season=="Pre-moult",]$vo2_l)
rest.summer <- mean(restData[restData$id=="Ronnie"&restData$season=="Moult",]$vo2_l)



##Make summary data files

proportions<-NULL
summary_output<-NULL
all_data<-NULL

SealFiles<-list.files("data/labelled_wilddata_nofeature/")

for(i in 1:length(SealFiles)){

load(paste0("data/labelled_wilddata_nofeature/",SealFiles[i]))

cat(paste0("data/labelled_wilddata_nofeature/",SealFiles[i]),"\n")

acc$EventType<-as.factor(paste0(acc$nPx,".",acc$finalPreds))

Seal<-substr(SealFiles[i],1,nchar(SealFiles[i])-6)

##Events that occurred for less than 7 second make the event before
x<-data.frame(unclass(rle(as.character(acc$EventType))))
x$other<-ifelse(x$lengths<15,x$values==NA,x$values)
acc$Events<-rep(x$other,x$lengths)
acc$Events<-replace_na_last(acc$Events)
acc$Events<-factor(acc$Events,levels=c(1,2,3,4,5,6,7,8,9,10,11,12),
                   labels = c("land.Foraging","land.Grooming","land.Resting",
                                         "land.Travelling","surface.Foraging","surface.Grooming",
                                         "surface.Resting","surface.Travelling","underwater.Foraging",
                                         "underwater.Grooming","underwater.Resting","underwater.Travelling"))

##relabel events
#change underwater grooming to foraging
levels(acc$Events)[levels(acc$Events)=="underwater.Grooming"]<-"underwater.Foraging"
x<-rle(as.character(acc$Events))
acc$FinalEvents<-rep(1:length(x$values),x$lengths)

###Make summary proportions table for plotting
loc.type<-table(acc$nPx,acc$finalPreds)

loc.type.prop<-melt(loc.type)
loc.type.prop$Seal<-Seal

proportions<-rbind(proportions,loc.type.prop)


####Run the energy budget
acc<- data.frame(acc)  

Events<-unique((acc$FinalEvents))

#threshold<-0.1

###Calculate the total energy expenditure
output<-NULL

toy<-acc$season[1]


  for(l in 1:length(Events)){
    dat<-acc[acc$FinalEvents==Events[l],]
    
    #EventType <- Mode(dat$EventType)
    EventType <- as.character(dat$EventType[1])
    Etemp <- mean(dat$temp,na.rm=TRUE)
    Etime <- nrow(dat)/60
    Erows <- nrow(dat)
    #dba<-dat[dat$VeDBA>threshold,]
    VeDBA.auc   <- MESS::auc(dat$date,   dat$VeDBA)  
    
    if(toy=="winter"){
    
    if(EventType=="surface.Foraging"){
      EE<-active*Etime
    }else if(EventType=="surface.Travelling"){
      EE<-active*Etime
    }else if(EventType=="underwater.Foraging"){
      EE<-active*Etime
    }else if(EventType=="underwater.Travelling"){
      EE<-active*Etime
    }else if(EventType=="land.Foraging"){
      EE<-active*2*Etime
    }else if(EventType=="land.Travelling"){
      EE<-active*2*Etime
    }else if(EventType=="surface.Grooming"){
      EE<-rest.winter*2*Etime
    }else if(EventType=="land.Grooming"){
      EE<-rest.winter*Etime
    }else if(EventType=="surface.Resting"){
      if(is.nan(Etemp)){
        EE<-rest.winter*Etime
      }else{
        EE<-(resting.lm$coefficients[1]+resting.lm$coefficients[2]*Etemp)*1.6
      }
    }else if(EventType=="underwater.Resting"){
      if(is.nan(Etemp)){
        EE<-rest.winter*Etime
      }else{
        EE<-(resting.lm$coefficients[1]+resting.lm$coefficients[2]*Etemp)*1.6
      }
    } else if(EventType=="land.Resting"){
      EE<-rest.winter*0.7*Etime
    }
      
    }else{
    
    
    if(EventType=="surface.Foraging"){
      EE<-active*Etime
    }else if(EventType=="surface.Travelling"){
      EE<-active*Etime
    }else if(EventType=="underwater.Foraging"){
      EE<-active*Etime
    }else if(EventType=="underwater.Travelling"){
      EE<-active*Etime
    }else if(EventType=="land.Foraging"){
      EE<-active*2*Etime
    }else if(EventType=="land.Travelling"){
      EE<-active*2*Etime
    }else if(EventType=="surface.Grooming"){
      EE<-rest.summer*2*Etime
    }else if(EventType=="land.Grooming"){
      EE<-rest.summer*Etime
    }else if(EventType=="surface.Resting"){
      if(is.nan(Etemp)){
        EE<-rest.summer*Etime
      }else{
        EE<-(resting.lm$coefficients[1]+resting.lm$coefficients[2]*Etemp)*1.6
      }
    }else if(EventType=="underwater.Resting"){
      if(is.nan(Etemp)){
        EE<-rest.summer*Etime
      }else{
        EE<-(resting.lm$coefficients[1]+resting.lm$coefficients[2]*Etemp)*1.6
      }
    } else if(EventType=="land.Resting"){
      EE<-rest.summer*0.7*Etime
    } 
  }##end else
    new.dat<-cbind(Events[i],EventType,VeDBA.auc,EE,Etime,Etemp)
    output<-rbind(output,new.dat)
}#end for l
      
  
##clean the output table
output<-data.frame(output,stringsAsFactors = FALSE)
output$EventType<-factor(output$EventType)
output$VeDBA.auc<-as.numeric(output$VeDBA.auc)
output$EE<-as.numeric(output$EE)
output$Etime<-as.numeric(output$Etime)
output$Etemp<-as.numeric(output$Etemp)

output$duration<-cumsum(output$Etime)
output$days<-as.integer(output$duration/1440)

Event<-unlist(strsplit(as.character(output$EventType),"[.]"))
output$Event<-Event[seq(2, length(Event),2)]
output$Place<-Event[seq(1, length(Event),2)]

output$Seal<-Seal

all_data<-rbind(all_data,output)

##summarise the energy output
output<-output[complete.cases(output$EE),]
kj_calc<-function(x){sum(x)/1000*24*5*4.186}
  
kj_per_day<-tapply(output$EE,output$days, kj_calc)
daily_activity<-table(output$days,output$Event)
DA<-melt(daily_activity)
DA$Var.2<-factor(DA$Var.2,levels = c("Resting","Grooming","Foraging","Travelling"))
DA<-DA[order(DA$Var.2),]
daily_activity_prop<-prop.table(daily_activity,1)

final_table<-data.frame(cbind(daily_activity_prop,kj_per_day))
final_table$days<-as.numeric(row.names(final_table))
final_table$Seal<-Seal

summary_output<-rbind(summary_output,final_table)

}#end for i

save(proportions,file="output/proportions_revised.RData")
save(all_data,file="output/all_data_revised.RData")
save(summary_output,file="output/summary_output_revised.RData")
