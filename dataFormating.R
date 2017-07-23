library(curl)

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

y2 <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/nonMelanomaCounts.csv"),
               header=TRUE)

dataFileRates <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/Allt3.csv"),
                          header=F,sep=";",skip=2,encoding="UTF-8")
dataFileCounts <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/Allt2.csv"),
                          header=F,sep=";",skip = 2,encoding="UTF-8")

#dataFileRates<-read.csv("C:/Users/Sebastian/Dropbox/Melt/Allt3.csv",header=F,sep=";",skip=2,encoding="UTF-8")
#dataFileCounts<-read.csv("C:/Users/Sebastian/Dropbox/Melt/Allt2.csv",header=F,sep=";",skip=2,encoding="UTF-8")

# cancerDataRates will be used for exploratory data analysis and cancerDataCounts will be used for 
# modeling.
cancerDataRates <- formatDF(dataFileRates)
cancerDataCounts <- formatDF(dataFileCounts)

# We write the data to new files.
write.csv(cancerDataRates,"C:/Users/Sebastian/Dropbox/Melt/nonMelanomaRates.csv",fileEncoding ="UTF-8")
write.csv(cancerDataCounts,"C:/Users/Sebastian/Dropbox/Melt/nonMelanomaCounts.csv",fileEncoding = "UTF-8")
# Couldn't find a way to write the files to my GitHub account so I saved them in a local folder and then
# uploaded them with git bash. You can download the files from my GitHub account (see dataAnalysis.R and
# Modeling.R) and skip this step.

#write.csv(cancerDataCounts,curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/Allt4.csv"),
          #fileEncoding = "UTF-8")
