formatDF<-function(dataFrame){
  # input: dataFrame is a data.frame object which we want to format in a certain way
  
  # output: returns a data.frame with the formatting we want
  res <- dataFrame
  res <- res[1:(dim(res)[1]-1),]
  colnames(res)<-c("Year","County","Age","Men","Women")
  
  # "Duplicate" the data frame, have to change the values
  nonSex <- subset(res,select=-c(Men,Women))
  nonSex[nrow(nonSex)+1:nrow(nonSex),] <- nonSex
  
  # Convert "Year" to numeric
  nonSex$Year <- as.numeric(nonSex$Year) + rep(1969,length(nonSex$Year))
  
  ageGroupFormat <- function(ageString){
    if(substr(ageString,nchar(ageString),nchar(ageString)) == "+"){
      # Covers the "85+"-case
      return(substr(ageString,1,3))
    }
    if(nchar(ageString) == 3){
      # For the youngest groups
      return(paste0(substr(ageString,1,1),"-",substr(ageString,3,3)))
    }
    else{
      # For the other groups
      return(paste0(substr(ageString,1,2),"-",substr(ageString,4,5)))
    }
  }
  
  nonSex$Age <- as.character(res$Age)
  nonSex$Age <- sapply(nonSex$Age,ageGroupFormat)
  nonSex$Age <- factor(nonSex$Age)
  
  countyFormat <- function(countyString){
    if(countyString == "Riket"){
      return("Sweden")
    }
    # Remove eventual " län" in the county name
    countyName <- substr(countyString,1,nchar(countyString)-4)
    # Remove the last "s", e.g. "Hallands" becomes "Halland"
    if(substr(countyName,nchar(countyName),nchar(countyName)) == "s"){
      countyName <- substr(countyName,1,nchar(countyName)-1)
    }
    # Remove non-latin symbols
    countyName <- gsub("å","a",countyName)
    countyName <- gsub("Å","A",countyName)
    countyName <- gsub("Ä","a",countyName)
    countyName <- gsub("Ö","O",countyName)
    countyName <- gsub("ä","a",countyName)
    countyName <- gsub("ö","o",countyName)
    return(countyName[[1]])
  }
  
  nonSex$County <- as.character(nonSex$County)
  nonSex$County <- as.factor(sapply(nonSex$County,countyFormat))
  
  # Format the response variables
  res$Men <- gsub(",",".",res$Men)
  res$Men <- gsub(" ","",res$Men)
  res$Men <- as.numeric(res$Men)
  res$Women <- gsub(",",".",res$Women)
  res$Women <- gsub(" ","",res$Women)
  res$Women <- as.numeric(res$Women)
  response <- c(res$Men,res$Women)
  
  sexInd <- c(rep("Men",length(res$Men)),rep("Women",length(res$Women)))
  
  result <- cbind(nonSex,sexInd,response)
  colnames(result) <- c("years","counties","age","sex","cases")
  return(result)
}