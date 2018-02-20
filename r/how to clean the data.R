rm(list=ls())
####Importing data order

####20 Hz data

#read the raw data and import
source('r/wild/read_data_clean.R')

#this exports to:
#"data/wilddata/"

#add place to 20Hz data for error checking - most of the time should be in the water
source('r/wild/add_place_run.r')

#This file contains all of the dive when the fast log was on

#A file to look at the unusual dives

source('r/wild/plot_unusual_dives.R')
plot_unusual_dives()

###make feature data for models
source('r/models/featureProcessing_wild20Hz.R')
featureProcessing(epochs=75)


#####1Hz data
#read the raw data and import
source('r/wild/read_data_1Hz.R')

#this exports to:
#"data/wilddata_1Hz/"

#remove times that cannot be used for analysis
#times before and after the seal was wearing the acc
#times when the fastrate log and/or the wet/dry sensor did not work
source('r/wild/cleaning_wild_files.R')
clean_1Hz()

#Takes a long time!!!

#this exports to:
#"output/1Hz_dives/"

###check out data manually - strange start times or events
source('r/wild/exploring_wild_1Hz.R')

#adding place and label events for 1Hz data
source('r/wild/add_place_run_1Hz.r')
add_place_1Hz()


