add_place_1Hz<-function(dat){
  

library(data.table)
source("r/functions.R")

##### set your working directory here
#setwd("~/Documents/r_files/Seals/wild_modles/")

##### you need rTools and a c++ compiler installed for this so compile

Rcpp::sourceCpp('r/wild/add_place.cpp')

##### load the wet/dry data
files<-list.files("data/wilddata_1Hz/")
wet.files<-list.files("output/wetdry_times/")

#i=5
for(i in 1:length(files)){

#  load the data
load(paste0("output/wetdry_times/",files[i]))

##### double check the data is ordered
wet_dry_times <- wet_dry_times[order(start_time)]

##### load the "raw data"
load(file = paste0("output/1Hz_dives/",files[i]))

##label events
dat<-label_events(dat)

#file must start after wet/dry starts
test<-data.frame(wet_dry_times)
start.time<-test[1,1]
start<-dat[1,1]

if(start.time>start){
  dat<-dat[dat$date>start.time,]
}

df <- subset(dat,select=c(date,x,y,z,event,depth,temp))
df<-as.data.table(df)
df<-df[order(date)]
cat(paste0("data/wilddata/",files[i]),"\n")


##### truncating the time stamps to seconds and converting to integer, see above comment re milliseconds
df[, time_stamp := as.integer(floor(as.numeric(date)))]


##### only using the start_time
# start_date vector passes is one longer than the wet/dry start_date vector so there is an end comparrison and
# needs to be after the final time to check in the raw_data
raw_place <- add_place(Place = wet_dry_times[, Place], 
                       start_date = c(wet_dry_times[, start_time], df[.N, time_stamp] + 1),
                       time_stamp = df[, time_stamp])

###### add the place vector, and we are done!
df[, Place := raw_place]

df<-subset(df,select=c(date,x,y,z,event,depth,temp,time_stamp,Place))

save(df,file=paste0("output/1Hz_dives/",files[i]))
}

}
