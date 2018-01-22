library(curl)
source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/Revised/dataFormating.R")
nonMelanomaRates <- formatDF(read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/Revised/rates.csv"),
                                      header=F,sep=";",skip=2,encoding="UTF-8"))

# There's a problem with the age groups, without the three lines below
# some age groups are not recognized when using e.g. timeDevelopment(), even
# though they look the same. This is a duct tape fix which will have to be
# taken care of later.
lst<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
age <- rep(factor(lst,levels=lst),nrow(nonMelanomaRates)/length(lst))
nonMelanomaRates$age <- age

colorVector <- c("#0048BA","#4C2F27","#B0BF1A","#7CB9E8","#B284BE",
                 "#5D8AA8","#AF002A","#84DE02","#E32636","#C46210",
                 "#E52B50","#FF2800","#F19CBB","#AB274F","#3B7A57",
                 "#00C4B0","#FFBF00","#00FFFF","#E0218A","#000000","#88540B","#FFF600")

timeDevelopment<-function(dataFrame,county,ageGroup){
  # input: "dataFrame" is a data.frame containing our data, "county" is a county, 
  # "ageGroup" is an age group and "sex" is the sex we are interested in
  
  # output: Produces a plot which shows the development over time of the number of cases per 100 000 inhabitants
  # of non-melanoma cancer in a specified county for a certain sex and given age group using a given data.frame
  
  dataFrameSubset <- dataFrame[which(dataFrame$counties == county & dataFrame$age == ageGroup),]
  men <- dataFrameSubset[which(dataFrameSubset$sex == "Men"),]
  women <- dataFrameSubset[which(dataFrameSubset$sex == "Women"),]
  count <- men$cases + women$cases
  plot(unique(dataFrameSubset$years),count,xlab="Years",ylab="New cancer cases per 100 000 inhabitants",
       main=paste("County: ",county,", Age group: ",ageGroup))
}

timeDevelopment(nonMelanomaRates,"Halland","85+")
timeDevelopment(nonMelanomaRates,"Halland","50-54")
timeDevelopment(nonMelanomaRates,"Halland","60-64")
timeDevelopment(nonMelanomaRates,"Sweden","85+")
timeDevelopment(nonMelanomaRates,"Skåne","85+")
timeDevelopment(nonMelanomaRates,"Norrbotten","85+")

countyComparisonBarPlot <- function(dataFrame){
  # input: dataFrame is a data.frame object containing our data
  
  # output: Produces a bar plot showing the average number of cases between 1970-2015
  # for the counties in dataFrame
  subDataFrame <- subset(dataFrame,dataFrame$counties != "Sweden")
  subDataFrame$counties <- factor(subDataFrame$counties)
  
  groups <- c(numeric(length(levels(subDataFrame$counties))))
  counties <- levels(subDataFrame$counties)
  
  for(i in 1:length(counties)){
    currentcounty <- subDataFrame[which(subDataFrame$counties == counties[i]),]
    groups[i] <- mean(currentcounty$cases)
  }
  
  op <- par(mar = c(8,4,4,2) + 0.1)
  barplot(groups,col=colorVector,main="Counties shown below",
          ylab="average number of cases per 100 000 inhabitants between 1970-2015",names.arg=counties,las=2)
  par(op)
}

countyComparisonBarPlot(nonMelanomaRates)

countyComparisonBoxPlot <- function(dataFrame){
  subDataFrame <- subset(dataFrame,dataFrame$counties != "Sweden")
  subDataFrame$counties <- factor(subDataFrame$counties)
  regions <- levels(subDataFrame$counties)
  n <- length(regions)
  tempVec <- c()
  for(i in 1:n){
    county <- regions[i]
    vectorOfCases <- subDataFrame[which(subDataFrame$counties == county),]$cases
    tempVec <- cbind(tempVec,vectorOfCases)
  }
  boxplot(tempVec,names=regions,las=2,cex.axis=0.7,
          main="Counties shown below",
          ylab = "Number of cases per 100 000 inhabitants")
}

countyComparisonBoxPlot(nonMelanomaRates)

# Shows the percentage of zeros
zeros<-nonMelanomaRates[which(nonMelanomaRates$cases==0),]$cases
percentageOfZeros <- length(zeros)/nrow(nonMelanomaRates)

ageGroupComparison <- function(dataFrame,county,colVec){
  # input: "dataFrame" is data.frame containing our data, "county" is our county of interest and "colVec" is a vector
  # containing different colors.
  
  # output: Produces a bar plot showing the average number of cases of non-melanoma skin cancer during 1970-2015
  # in all our age groups in our given county.
  
  dataFrameSubset <- dataFrame[which(dataFrame$counties == county),]
  groups <- c(numeric(length(levels(dataFrameSubset$age))))
  ages <- levels(dataFrameSubset$age)
  
  n <- length(ages)
  for(i in 1:n){
    currentGroup <- dataFrameSubset[which(dataFrameSubset$age == ages[i]),]
    groups[i] <- median(currentGroup$cases)
  }
  
  barplot(groups,col=colorVector,main=paste(county," Both sexes"),xlab="Age groups",
          ylab="Median number of cases per 100 000 inhabitants between 1970-2015",
          names.arg = ages)
  legend("topleft",legend=ages,fill=colVec,ncol=3)
}

allCounties<-levels(nonMelanomaRates$counties)

for(x in allCounties){
  ageGroupComparison(nonMelanomaRates,x,colorVector)
}
# Shows that non-melanoma skin cancer is very rare in some age groups.

for(x in allCounties){
  timeDevelopment(nonMelanomaRates,x,"85+")
}

ageGroupComparisonTime<-function(dataFrame,county,ageGroups,colVec){
  yearsHere <- rev(unique((dataFrame$years)))
  n <- length(ageGroups)
  
  subDataFrame <- dataFrame[which(dataFrame$counties == county),]
  men <- subDataFrame[which(subDataFrame$sex == "Men"),]
  women <- subDataFrame[which(subDataFrame$sex == "Women"),]
  maxVal <- max(c(men$cases,women$cases))
  minVal <- min(c(men$cases,women$cases))
  
  plot(0,0,xlim=c(1970,2015),ylim=c(minVal,maxVal),main = paste("Both sexes, county: ",county),
       xlab = "Years",ylab="Number of cases per 100 000 inhabitants")
    
  for(i in 1:n){
    menInAgeGroup <- men[which(men$age == ageGroups[i]),]
    womenInAgeGroup <- women[which(women$age == ageGroups[i]),]
    totalNumber2 <- menInAgeGroup$cases + womenInAgeGroup$cases
    lines(yearsHere,rev(totalNumber2),col=colVec[i])
  }
  legend('topleft',legend=ageGroups,col=colVec,pch=1,lty=c(1,1))
}

ageGroupComparisonTime(nonMelanomaRates,"Blekinge",c("0-4","50-54","80-84"),colorVector)
ageGroupComparisonTime(nonMelanomaRates,"Sweden",c("0-4","60-64","80-84"),colorVector)

countyComparisonTime<-function(dataFrame,comparisonCounties,ageGroup,colVec){
  # input: "dataFrame" is the data.frame we will be using, "counties" is a vector containing the
  # counties we will compare to one another, "ageGroup" is the age group we are interested in,
  # "sex" is the sex we are interested in and "colVec" is a vector of colors we will use to color
  # the lines
  
  # output: Produces a plot of lines, one plot for each county, between 1970 and 2015 for our given
  # age group and sex.
  yearsHere <- rev(unique((dataFrame$years)))
  n <- length(comparisonCounties)
  
  subDataFrame <- dataFrame[which(dataFrame$age == ageGroup),]
  subDataFrame <- subDataFrame[subDataFrame$counties %in% comparisonCounties,]
  men <- subDataFrame[subDataFrame$sex == "Men",]$cases
  women <- subDataFrame[subDataFrame$sex == "Women",]$cases

  maxVal <- max(men+women)
  minVal <- min(men+women)
  plot(0,0,xlim=c(1970,2015),ylim=c(minVal,maxVal),main = paste("Both sexes, age group: ",ageGroup),
       xlab = "Years",ylab="Number of cases per 100 000 inhabitants")
  
  for(i in 1:n){
    regSpec <- subDataFrame[subDataFrame$counties == comparisonCounties[i],]
    men <- regSpec[which(regSpec$sex == "Men"),]
    women <- regSpec[which(regSpec$sex == "Women"),]
    totalNumber <- men$cases + women$cases
    lines(yearsHere,rev(totalNumber),col=colVec[i])
  }
  legend('topleft',legend=comparisonCounties,col=colVec,pch=1,lty=c(1,1))
}

countyComparisonTime(nonMelanomaRates,c("Halland", "Stockholm", "Skåne"),"85+",colorVector)
countyComparisonTime(nonMelanomaRates,c("Blekinge", "Västerbotten", "Jönköping"),"70-74",colorVector)
countyComparisonTime(nonMelanomaRates,c("Halland","Blekinge"),"85+",colorVector)

# The grouping of counties is arbitrary and can be done in other ways, I tried grouping them "horizontally" in a
# geographic sense.
region1 <- c("Skåne","Blekinge")
region2 <- c("Kalmar","Kronoberg","Halland")
region3 <- c("Gotland","Jönköping","Västra Götaland","Östergötland")
region4 <- c("Södermanland","Örebro","Värmland")
region5 <- c("Stockholm","Västmanland","Uppsala")
region6 <- c("Dalarna","Gävleborg")
region7 <- c("Jämtland", "Västernorrland")
region8 <- c("Västerbotten","Norrbotten")
allRegions <- list(region1,region2,region3,region4,region5,region6,region7,region8)

# Plots for men
for(x in allRegions){
  countyComparisonTime(nonMelanomaRates,x,"85+",colorVector)
}

# Plots for women
for(x in allRegions){
  countyComparisonTime(nonMelanomaRates,x,"85+",colorVector)
}

sumGroups<-function(subCancerData){
  # input: "subCancerData" is a data.frame, in our case we will let it be a subset of a
  # larger data.frame
  
  # output: returns a vector whose elements are the sums of all the cases per 100 000 
  # inhabitants for all our age groups in a given year, e.g. groups 2, 7, 12, ... , 85+
  # summed together for all the years 1970,...,2015
  
  yearVec <- rev(unique(subCancerData$years))
  n <- length(yearVec)
  res <- c(numeric(n))
  
  for(i in 1:n){
    currentYear <- subCancerData[which(subCancerData$years == yearVec[i]),]
    temp <- head(currentYear,1)
    temp$cases <- sum(head(currentYear$cases,length(levels(subCancerData$age))))
    res[i] <- temp$cases
  }
  return(res)
}

sexComparison <- function(dataFrame,county,ageGroup){
  # input: "dataFrame" is a data.frame containing our data, "county" is our county of
  # interest and "ageGroup" is the age group in which we are intersted in the differences
  # between the sexes
  
  # output: Produces a line plot showing the number of cases per 100 000 inhabitants for
  # both sexes
  
  men <- dataFrame[which(dataFrame$sex == "Men" & dataFrame$counties == county & dataFrame$age == ageGroup),]
  women <- dataFrame[which(dataFrame$sex == "Women" & dataFrame$counties == county & dataFrame$age == ageGroup),]
  menCount <- sumGroups(men)
  womenCount <- sumGroups(women)
  yearVec <- rev(unique(dataFrame$years))
  
  maxVal <- max(max(menCount),max(womenCount))
  minVal <- min(min(menCount),min(womenCount))
  
  plot(yearVec,menCount,type="l",col="blue",xlab="Years",ylab="Number of new cases per 100 000 inhabitants",
       ylim = c(minVal,maxVal),
       main=paste(county, ", Age group: ",ageGroup))
  lines(yearVec,womenCount,type="l",col="red")
  legend('topleft',legend=c("Men","Women"),col=c("blue","red"),pch=1,lty=c(1,1))
}

comparisonRegions <- c("Halland", "Sweden", "Skåne", "Västra Götaland")

for(x in lst[11:18]){
  for(y in allCounties){
    sexComparison(nonMelanomaRates,y,x)
  }
}