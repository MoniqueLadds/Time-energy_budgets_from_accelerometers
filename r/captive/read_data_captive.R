source("R\\functions.R")
library(zoo)
library(pracma)
library(MASS)
library(KernSmooth)
library(diveMove)
library(lubridate)
library(data.table)

filename <- "data//Captive//Rocky//Rocky A09863_17-07-13.csv"
data <- readLines(filename)

blank <- which(data == "")

## Start and end of interesting blocks of data, ignoring the blank rows.
block.start <- blank[diff(c(blank, Inf)) > 1] + 1
## This is a hack.  Think about how to do it more nicely.
block.start <- c(1, block.start[-length(block.start)])
block.end   <- blank[diff(c(0, blank)) > 1] - 1

block.first <- data[block.start]

## These are regular expressions.  See ?regexp
re.data <- "^Data Block ([0-9]+)$"
re.log <- "^Fast Log ([0-9]+)$"

idx.data <- grep(re.data, block.first)
idx.log <- grep(re.log, block.first)

## as.integer(sub(re.data, "\\1", block.first[idx.data]))
## as.integer(sub(re.log, "\\1", block.first[idx.log]))

## Get all the data.  Will take about a minute per set, perhaps a bit
## longer for the third one.

contents.data <- lapply(idx.data, function(i)
                        read.data(data.sub(i, block.start, block.end, data)))

##Check read.data works
x <- read.data(data.sub(idx.log[1], block.start, block.end, data))
write.csv(x,"data//Captive//Rocky//Raw//Rocky A09863_17-07-13.csv",row.names=FALSE)

