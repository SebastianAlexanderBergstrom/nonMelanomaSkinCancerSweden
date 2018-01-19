formatDF<-function(dataFrame){
  # input: dataFrame is a data.frame object which we want to format in a certain way
  
  # output: returns a data.frame with the formatting we want
  res <- dataFrame
  colnames(res)<-c("Year","County","Age","Men","Women")
  
  # "Duplicate" the data frame, have to change the values
  nonSex <- subset(res,select=-c(Men,Women))
  nonSex[nrow(nonSex)+1:nrow(nonSex),] <- nonSex
  
  # Convert "Year" to numeric
  nonSex$Year <- as.numeric(nonSex$Year)
  
  ageGroupFormat <- function(ageString){
    if(nchar(ageString) == 2){
      return(paste(substr(ageString,1,1),"-",substr(ageString,2,2),sep=""))
    }
    else{
      return(paste(substr(ageString,1,2),"-",substr(ageString,3,5),sep=""))
    }
  }
  
  nonSex$Age <- as.character(res$Age)
  nonSex$Age <- as.factor(sapply(nonSex$Age,ageGroupFormat))
  
  countyFormat <- function(countyString){
    if(countyString == "Riket"){
      return("Sweden")
    }
    countyName <- strsplit(countyString,split=" ")[[1]][1]
    # Remove the last "s", e.g. "Hallands" becomes "Halland"
    if(substr(countyName,nchar(countyName),nchar(countyName)) == "s"){
      countyName <- substr(countyName,1,nchar(countyName)-1)
    }
    return(countyName[[1]])
  }
  
  nonSex$County <- as.character(nonSex$County)
  nonSex$County <- as.factor(sapply(nonSex$County,countyFormat))
  
  # Format the response variables
  response <- c(res$Men,res$Women)
  response <- gsub(",",".",response)
  response <- gsub(" ","",response)
  response <- as.numeric(response)
  
  # Indicator for sex
  #men <- rep(1,nrow(res))
  #women <- rep(0,nrow(res))
  #sexInd <- c(men,women)
  
  # Can also use this
  sexInd <- c(rep("Men",length(res$Men)),rep("Women",length(res$Women)))
  
  result <- cbind(nonSex,sexInd,response)
  return(result)
}