formatDF<-function(dataFrame){
  # input: dataFrame is a data.frame object which we want to format in a certain way
  
  # output: returns a data.frame with the formatting we want
  res <- dataFrame
  res <- res[1:(dim(res)[1]-1),]
  #print(summary(res))
  colnames(res)<-c("Year","County","Age","Men","Women")
  
  # "Duplicate" the data frame, have to change the values
  nonSex <- subset(res,select=-c(Men,Women))
  nonSex[nrow(nonSex)+1:nrow(nonSex),] <- nonSex
  
  # Convert "Year" to numeric
  # Blir ett fel här som sedan kommer och hemsöker dig i funktionerna, ska ge talen 1970-2015 men ger 1-46
  nonSex$Year <- as.numeric(nonSex$Year) + rep(1969,length(nonSex$Year))
  #print(as.numeric(unique(nonSex$Year)))
  
  ageGroupFormat <- function(ageString){
    #print(nchar(ageString)) #nchar(ageString) is NA
    # Lägg till en if-sats som tar hand om "85+"
    if(nchar(ageString) == 2){
      # For the youngest groups
      return(paste(substr(ageString,1,1),"-",substr(ageString,2,2),sep=""))
    }
    if(substr(ageString,nchar(ageString),nchar(ageString)) == "+"){
      # Think this should cover the "85+"-case
      return(substr(ageString,1,3))
    }
    else{
      # For the other group
      return(paste(substr(ageString,1,2),"-",substr(ageString,3,5),sep=""))
    }
  }
  
  nonSex$Age <- as.character(res$Age)
  nonSex$Age <- as.factor(sapply(nonSex$Age,ageGroupFormat))
  nonSex$Age <- toString(nonSex$Age)
  #nonSex$Age <- as.factor(nonSex$Age)
  
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
    return(countyName[[1]])
  }
  
  nonSex$County <- as.character(nonSex$County)
  nonSex$County <- as.factor(sapply(nonSex$County,countyFormat))
  
  # Format the response variables
  # Blir fel någonstans längs de här raderna
  #print(res$Men)
  #print(res$Women)
  # res$Men, res$Women var för sig verkar funka men så fort du skapar "response" skiter det sig
  #response <- c(res$Men,res$Women)
  #response <- gsub(",",".",response)
  #response <- gsub(" ","",response)
  #response <- as.numeric(response)

  res$Men <- gsub(",",".",res$Men)
  res$Men <- gsub(" ","",res$Men)
  res$Men <- as.numeric(res$Men)
  #print(res$Men)
  res$Women <- gsub(",",".",res$Women)
  res$Women <- gsub(" ","",res$Women)
  res$Women <- as.numeric(res$Women)
  #print(res$Women)
  response <- c(res$Men,res$Women)

    
  # Indicator for sex
  #men <- rep(1,nrow(res))
  #women <- rep(0,nrow(res))
  #sexInd <- c(men,women)
  
  # Can also use this
  sexInd <- c(rep("Men",length(res$Men)),rep("Women",length(res$Women)))
  
  result <- cbind(nonSex,sexInd,response)
  colnames(result) <- c("years","counties","age","sex","cases")
  return(result)
}