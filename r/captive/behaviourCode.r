behaviourCode <- function(inputData){
  
  ##### the input data just needs the raw column entries and the first location
  # entries in other columns will likely cause unxepected behaviour
  # I haven't written any error handling or input checking yet as I want to go to the gym!
  # I can do that tomorrow.......
  
  ##### load input Data for testing
#   if(.Platform$OS.type=="unix"){
#     #MAC
#     behaviourCodeTable <- read.csv(file="~/Google Drive/Behaviour accelerometry/Behaviour codes2.csv", stringsAsFactors=FALSE)      
#   }else{
#     #PC
#     behaviourCodeTable <- read.csv(file="C:/users/admin/Google Drive/Behaviour accelerometry/Behaviour codes2.csv", stringsAsFactors=FALSE)  
#   }
  #inputData=read.csv(file="~/Google Drive/Behaviour accelerometry/data/raw data example2.csv", stringsAsFactors=FALSE) 
  fileLoc = paste0("data/",family,"/behaviour_20Hz/")
  # setwd(fileLoc)
  behaviourCodeTable <- read.csv("behaviourCodes2.csv", stringsAsFactors=FALSE)  
  
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
#   browser()
  #inputData$behaviour <- rep(inputData$raw[ind],
   #                          times = diff(   
  #                             c(ind, length(inputData$raw) + 1) )
  #)
  
  ##### fill in type from behaviourCodeTable
#   unique(inputData$behaviour)
#  browser()
  #for(xx in unique(inputData$behaviour)){
#      cat(xx,"\n")
#      browser()
   # inputData$type[inputData$behaviour==xx] <- behaviourCodeTable[behaviourCodeTable[,1]==xx,2][1]
  #}
  
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
  surfaceIdx <- inputData$depth < 1.7 & inputData$location == "water"
  inputData$place[surfaceIdx] <- "surface"
  
  # everything else in underwater
  inputData$place[!(surfaceIdx + landIdx)] <- "underwater"
  
  # type_event is numbered type
  inputData$type_event <- paste(inputData$value, ".", inputData$type, sep="")
  
  # behaviour_event count of each event
  inputData$behaviour_event <- paste(inputData$number, ".", inputData$behaviour, sep="")
  
  return(inputData)
  
}




