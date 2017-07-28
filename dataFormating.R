formatDF<-function(dataFrame){
  # input: dataFrame is a data.frame object which we want to format in a certain way
  
  # output: returns a data.frame with the formatting we want
  res <- dataFrame
  
  # Removes the last row which contains a string without data
  res <- res[1:(dim(res)[1]-1),]
  
  colnames(res)<-c("Year","County","Age","Men","Women")
  
  res$Year <- as.numeric(as.character(res$Year))
  years<-rep(res$Year,2)
  
  lst<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
         "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
  age <- rep(factor(lst,levels=lst),2024)
  counties<-rep(res$County,2)
  
  # County names are translated to English and formated. We had some problems with Skane, Orebro,
  # Uppsala and Vastra Gotaland, the commands below fix those issues.
  counties <- gsub("s län","",counties)
  counties <- gsub("län","",counties)
  counties <- gsub(" ","",counties)
  counties <- gsub("Riket","Sweden",counties)
  
  # Remove commas and blank spaces for numerical valuess
  cases <- unlist(list(res$Men, res$Women))
  cases <- gsub(" ","",cases)
  cases <- gsub(",",".",cases)
  cases <- as.numeric(cases)
  
  n<-length(res$Men)
  sexInd <- c(rep("Men",n),rep("Women",n))
  
  res<-data.frame(years,age,counties,cases,sexInd)
  return(res)
}
