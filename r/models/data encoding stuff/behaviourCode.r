behaviourCode <- function(inputData,
                          freq = 25,
                          behaviourCodeTable,
                          FT = FALSE){
  
  ##### the input data just needs the raw column entries and the first location
  # entries in other columns will likely cause unxepected behaviour
  # I haven't written any error handling or input checking yet as I want to go to the gym!
  # I can do that tomorrow.......

    ##### trim data to colplete number of samples
  # inputData <- inputData[1:(floor(nrow(inputData)/freq)*freq),]
  
  # browser()
  
  ##### load input Data for testing
#   if(.Platform$OS.type=="unix"){
#     #MAC
#     
#     #     behaviourCodeTable <- read.csv(file="~/Google Drive/Behaviour accelerometry/Behaviour codes2.csv",
#     #                                    stringsAsFactors=FALSE)      
#   }else{
#     #PC
#     behaviourCodeTable <- read.csv(file="C:/users/admin/Google Drive/Behaviour accelerometry/Behaviour codes2.csv",
#                                    stringsAsFactors=FALSE)  
#   }
  #inputData=read.csv(file="~/Google Drive/Behaviour accelerometry/data/raw data example2.csv", stringsAsFactors=FALSE)  
  
  ##### add behaviour codes to raw accelermator data
  inputData$raw[grepl("finish", inputData$raw, ignore.case=TRUE)] <- ""
  #   inputData$behaviour[grepl("finish", inputData$behaviour, ignore.case=TRUE)] <- ""
  
  ##### remove any white space from the behaviours
  inputData$raw <- gsub("[[:space:]]", "", inputData$raw)
  inputData$raw <- gsub("'", "", inputData$raw)
  
  ##### replace incorrectly encoded behavoiurs
  inputData$raw <- gsub("outofcamera", "other", inputData$raw)
  inputData$raw <- gsub("glide", "swimming", inputData$raw)
  inputData$raw <- gsub("thrashing", "thrash", inputData$raw)
  inputData$raw <- gsub("pushing", "manipulation", inputData$raw)
  inputData$raw <- gsub("bottom", "other", inputData$raw)
  inputData$raw <- gsub("mnaipulation", "manipulation", inputData$raw)
  inputData$raw <- gsub("eat", "other", inputData$raw)
  
  inputData$raw <- gsub("target", "other", inputData$raw)
  
  
  #   browser()
  # copy down behavours first
  ind = which(!inputData$raw=="")      # get positions of nonmissing values
  
  # repeat the values at these indices
  inputData$behaviour <- rep(inputData$raw[ind],
                             times = diff(   
                               c(ind, length(inputData$raw) + 1) )
  )
  
  ##### fill in type from behaviourCodeTable
  #   unique(inputData$behaviour)
  #  browser()
  for(xx in unique(inputData$behaviour)){
    #      cat(xx,"\n")
    inputData$type[inputData$behaviour==xx] <- behaviourCodeTable[behaviourCodeTable[,1]==xx,2][1]
    # inputData$type[inputData$behaviour==xx] <- behaviourCodeTable[behaviourCodeTable[,1]==xx,3][1]
  }
  # browser()
  
  ##### fill in location
  # assumes that the first line in location is already filled in
  # only need to check where behaviour changes
  behaviourVec <- inputData$behaviour[ind]
  itt <- length(ind)
  ind <- c(ind, nrow(inputData))
  currentLocation <- inputData$location[1]
  for(i in 1:itt){
    inputData$value[ind[i]:(ind[i+1]-1)] <- i
    
    inputData$number[ind[i]:(ind[i+1]-1)] <- 
      sum(behaviourVec[1:i] == behaviourVec[i])
    
    inputData$location[ind[i]:(ind[i+1]-1)] <- currentLocation
    
    ##### check the next behaviour
    # if in or out we need to change from water to land
    # otherwise copy down behaviour (i.e. keep current behaviour)
    if(  inputData$behaviour[ind[i+1]] == "out"){
      currentLocation <- "land"}
    if(  inputData$behaviour[ind[i+1]] == "in"){
      currentLocation <- "water"}
    
  }
  
  ##### place
  # if location = land then land
  landIdx <- inputData$location=="land"
  inputData$place[landIdx] <- "land"
  
  # water
  ##### as depth increases they are underwater
  surfaceIdx <- inputData$depth < 1.05 & inputData$location == "water"
  inputData$place[surfaceIdx] <- "surface"
  
  # everything else in underwater
  inputData$place[!(surfaceIdx + landIdx)] <- "underwater"
  
  # type_event is numbered type
  inputData$type_event <- paste(inputData$value, ".", inputData$type, sep="")
  
  # behaviour_event count of each event
  inputData$behaviour_event <- paste(inputData$number, ".", inputData$behaviour, sep="")
  
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  # browser()
  nr <- nrow(inputData)
  value3 <- sort(rep(1:floor(nrow(inputData)/3), times = 3))
  inputData$value3 <- c(value3, rep.int(x = 0, nr - length(value3)))
  value5 <- sort(rep(1:floor(nrow(inputData)/5), times = 5))
  inputData$value5 <- c(value5, rep.int(x = 0, nr - length(value5)))
  value7 <- sort(rep(1:floor(nrow(inputData)/7), times = 7))
  inputData$value7 <- c(value7, rep.int(x = 0, nr - length(value7)))
  value9 <- sort(rep(1:floor(nrow(inputData)/9), times = 9))
  inputData$value9 <- c(value9, rep.int(x = 0, nr - length(value9)))
  value11 <- sort(rep(1:floor(nrow(inputData)/11), times = 11))
  inputData$value11 <- c(value11, rep.int(x = 0, nr - length(value11)))
  
    value13 <- sort(rep(1:floor(nrow(inputData)/13), times = 13))
  inputData$value13 <- c(value13, rep.int(x = 0, nr - length(value13)))
  value25 <- sort(rep(1:floor(nrow(inputData)/25), times = 25))
  inputData$value25 <- c(value25, rep.int(x = 0, nr - length(value25)))
  value75 <- sort(rep(1:floor(nrow(inputData)/75), times = 75))
  inputData$value75 <- c(value75, rep.int(x = 0, nr - length(value75)))
  inputData$type_event3 <- ""
  inputData$type_event5 <- ""
  inputData$type_event7 <- ""
  inputData$type_event9 <- ""
  inputData$type_event11 <- ""
  inputData$type_event13 <- ""
  inputData$type_event25 <- ""
  inputData$type_event75 <- ""
#   inputData$type_event13[seq(from = 1, to = floor(nrow(inputData)/freq)*freq , by = freq)] <-
#     tapply(X = inputData$type[1:(floor(nrow(inputData)/freq)*freq)],
#            INDEX = sort(rep(x = 1:floor(nrow(inputData)/freq), times = freq)),
#            FUN = Mode)
  # browser()
  inputData$type_event3[seq(from = 1, to = floor(nrow(inputData)/3)*3 , by = 3)] <-
    tapply(X = inputData$type[1:(floor(nrow(inputData)/3)*3)],
           INDEX = sort(rep(x = 1:floor(nrow(inputData)/3), times = 3)),
           FUN = Mode)
  inputData$type_event5[seq(from = 1, to = floor(nrow(inputData)/5)*5 , by = 5)] <-
    tapply(X = inputData$type[1:(floor(nrow(inputData)/5)*5)],
           INDEX = sort(rep(x = 1:floor(nrow(inputData)/5), times = 5)),
           FUN = Mode)
  inputData$type_event7[seq(from = 1, to = floor(nrow(inputData)/7)*7 , by = 7)] <-
    tapply(X = inputData$type[1:(floor(nrow(inputData)/7)*7)],
           INDEX = sort(rep(x = 1:floor(nrow(inputData)/7), times = 7)),
           FUN = Mode)
  inputData$type_event9[seq(from = 1, to = floor(nrow(inputData)/9)*9 , by = 9)] <-
    tapply(X = inputData$type[1:(floor(nrow(inputData)/9)*9)],
           INDEX = sort(rep(x = 1:floor(nrow(inputData)/9), times = 9)),
           FUN = Mode)
  inputData$type_event11[seq(from = 1, to = floor(nrow(inputData)/11)*11 , by = 11)] <-
    tapply(X = inputData$type[1:(floor(nrow(inputData)/11)*11)],
           INDEX = sort(rep(x = 1:floor(nrow(inputData)/11), times = 11)),
           FUN = Mode)
  inputData$type_event13[seq(from = 1, to = floor(nrow(inputData)/13)*13 , by = 13)] <-
    tapply(X = inputData$type[1:(floor(nrow(inputData)/13)*13)],
           INDEX = sort(rep(x = 1:floor(nrow(inputData)/13), times = 13)),
           FUN = Mode)
  inputData$type_event25[seq(from = 1, to = floor(nrow(inputData)/25)*25 , by = 25)] <-
    tapply(X = inputData$type[1:(floor(nrow(inputData)/25)*25)],
           INDEX = sort(rep(x = 1:floor(nrow(inputData)/25), times = 25)),
           FUN = Mode)
  inputData$type_event75[seq(from = 1, to = floor(nrow(inputData)/75)*75 , by = 75)] <-
    tapply(X = inputData$type[1:(floor(nrow(inputData)/75)*75)],
           INDEX = sort(rep(x = 1:floor(nrow(inputData)/75), times = 75)),
           FUN = Mode)
  
  ind3 = which(!inputData$type_event3=="")      # get positions of nonmissing values
  ind5 = which(!inputData$type_event5=="")      # get positions of nonmissing values
  ind7 = which(!inputData$type_event7=="")      # get positions of nonmissing values
  ind9 = which(!inputData$type_event9=="")      # get positions of nonmissing values
  ind11 = which(!inputData$type_event11=="")      # get positions of nonmissing values
  ind13 = which(!inputData$type_event13=="")      # get positions of nonmissing values
  ind25 = which(!inputData$type_event25=="")      # get positions of nonmissing values
  ind75 = which(!inputData$type_event75=="")      # get positions of nonmissing values
  
  # repeat the values at these indices
  inputData$type_event3 <- rep(inputData$type_event3[ind3],
                                times = diff(   
                                  c(ind3, length(inputData$type_event3) + 1) )
  )
  inputData$type_event5 <- rep(inputData$type_event5[ind5],
                                times = diff(   
                                  c(ind5, length(inputData$type_event5) + 1) )
  )
  inputData$type_event7 <- rep(inputData$type_event7[ind7],
                                times = diff(   
                                  c(ind7, length(inputData$type_event7) + 1) )
  )
  inputData$type_event9 <- rep(inputData$type_event9[ind9],
                                times = diff(   
                                  c(ind9, length(inputData$type_event9) + 1) )
  )
  inputData$type_event11 <- rep(inputData$type_event11[ind11],
                                times = diff(   
                                  c(ind11, length(inputData$type_event11) + 1) )
  )
  inputData$type_event13 <- rep(inputData$type_event13[ind13],
                                times = diff(   
                                  c(ind13, length(inputData$type_event13) + 1) )
  )
  inputData$type_event25 <- rep(inputData$type_event25[ind25],
                               times = diff(   
                                 c(ind25, length(inputData$type_event25) + 1) )
  )
  # browser()
  if(ind75[1] != 1) ind75 <- c(1, ind75)
  inputData$type_event75 <- rep(inputData$type_event75[ind75],
                               times = diff(   
                                 c(ind75, length(inputData$type_event75) + 1) )
  )
  inputData$type_event3 <- paste0(inputData$value3, ".", inputData$type_event3)
  inputData$type_event5 <- paste0(inputData$value5, ".", inputData$type_event5)
  inputData$type_event7 <- paste0(inputData$value7, ".", inputData$type_event7)
  inputData$type_event9 <- paste0(inputData$value9, ".", inputData$type_event9)
  inputData$type_event11 <- paste0(inputData$value11, ".", inputData$type_event11)
  inputData$type_event13 <- paste0(inputData$value13, ".", inputData$type_event13)
  inputData$type_event25 <- paste0(inputData$value25, ".", inputData$type_event25)
  inputData$type_event75 <- paste0(inputData$value75, ".", inputData$type_event75)
  
  if(FT){
    # browser()
    stftDataX <- data.frame(X = stft(inputData$x, win = freq, inc = freq)$values)
    stftDataX$MINX <- apply(X = stftDataX, MARGIN = 1, FUN = min)
    stftDataX$MAXX <- apply(X = stftDataX, MARGIN = 1, FUN = max)
    stftDataX$MEANX <- apply(X = stftDataX, MARGIN = 1, FUN = mean)
    stftDataX$SDX <- apply(X = stftDataX, MARGIN = 1, FUN = stats::sd)
    
    stftDataY <- data.frame(Y = stft(inputData$y, win = freq, inc = freq)$values)
    stftDataY$MINY <- apply(X = stftDataY, MARGIN = 1, FUN = min)
    stftDataY$MAXY <- apply(X = stftDataY, MARGIN = 1, FUN = max)
    stftDataY$MEANY <- apply(X = stftDataY, MARGIN = 1, FUN = mean)
    stftDataY$SDY <- apply(X = stftDataY, MARGIN = 1, FUN = stats::sd)
    
    stftDataZ <- data.frame(Z = stft(inputData$z, win = freq, inc = freq)$values)
    stftDataZ$MINZ <- apply(X = stftDataZ, MARGIN = 1, FUN = min)
    stftDataZ$MAXZ <- apply(X = stftDataZ, MARGIN = 1, FUN = max)
    stftDataZ$MEANZ <- apply(X = stftDataZ, MARGIN = 1, FUN = mean)
    stftDataZ$SDZ <- apply(X = stftDataZ, MARGIN = 1, FUN = stats::sd)
    
    # browser()
    
    stftData <- cbind(stftDataX, stftDataY, stftDataZ)
    stftData$type_event2 <- unique(inputData$type_event2)
    
    return(stftData)
  }else{
    return(inputData)
  }
}




