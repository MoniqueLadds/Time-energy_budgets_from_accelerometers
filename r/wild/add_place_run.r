######
rm(list = ls())
library(data.table)


##### set your working directory here
#setwd("~/Documents/r_files/Seals/wild_modles/")

##### you need rTools and a c++ compiler installed for this so compile
# Stu should be able to help with this
Rcpp::sourceCpp('r/wild/add_place.cpp')

##### load the wet/dry data
files<-list.files("data/wilddata/")
wet.files<-list.files("output/wetdry_times/")

#i=5
for( i in 1:length(files)){

#wet_dry_times <- fread(input = paste0("output/wetdry_times/",wet.files[i]),
 #                      col.names = c("start", "end", "Place"),
  #                     colClasses = c("character", "character", "NULL", "character"))

load(paste0("output/wetdry_times/",files[i]))

  ##### convert dates to intergers for comparrison
# if you want to use milliseconds you will need to replace as.integer with as.numeric and
# change the format in the call to as.POSIXct

wet_dry_times[, start_time := as.numeric(as.POSIXct(x = start, format = "%Y-%m-%d %H:%M:OS"))]
wet_dry_times[, end_time := as.integer(as.POSIXct(x = end, format = "%Y-%m-%d %H:%M:OS"))]

##### double check the data is ordered
wet_dry_times <- wet_dry_times[order(start_time)]

#save(wet_dry_times,file=paste0("output/wetdry_times/",files[i]))
##### if you are running out of memory uncomment and run the following line
# this will drop the start and end conumns
# wet_dry_times[, c("start", "end") := NULL]

# str(wet_dry_times)

##### load the "raw data"
load(file = paste0("data/wilddata/",files[i]))
df <- as.data.table(df)
cat(paste0("data/wilddata/",files[i]))

##### truncating the time stamps to seconds and converting to integer, see above comment re milliseconds
df[, time_stamp := as.integer(floor(as.numeric(date)))]

# str(df)

##### only using the start_time
# start_date vector passes is one longer than the wet/dry start_date vector so there is an end comparrison and
# needs to be after the final time to check in the raw_data
raw_place <- add_place(Place = wet_dry_times[, Place], 
                       start_date = c(wet_dry_times[, start_time], df[.N, time_stamp] + 1),
                       time_stamp = df[, time_stamp])

###### add the place vector, and we are done!
df[, Place := raw_place]

df<-subset(df,select=c(date,x,y,z,divenum,depth,temp,time_stamp,Place))

save(df,file=paste0("data/wilddata/",files[i]))
}


##### you don't need to run the below code
# this was to check the c++ code vs and R implementation
# worth running once to see for yourself that everything matches

##### check for expansion match
for(i in 1:(nrow(wet_dry_times) - 1)){
  cat(i,"\n")
  df[ time_stamp >= wet_dry_times$start_time[i] &
        time_stamp < wet_dry_times$start_time[i + 1],
      Place_check := wet_dry_times[i, Place]]
}
##### this should be 1
df[, mean(Place_check == Place)]

