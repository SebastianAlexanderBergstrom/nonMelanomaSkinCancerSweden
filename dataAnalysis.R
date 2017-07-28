# Provar att köra utan att skriva själva filen.
source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/dataFormating.R")
nonMelanomaRates <- formatDF(read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/Allt3.csv"),
                                      header=F,sep=";",skip=2,encoding="UTF-8"))

lst<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
age <- rep(factor(lst,levels=lst),2024)

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
timeDevelopment(nonMelanomaRates,"Sweden","85+")
timeDevelopment(nonMelanomaRates,"Skåne","85+")
timeDevelopment(nonMelanomaRates,"Norrbotten","85+")

countyComparisonBarPlot <- function(dataFrame){
  # input: dataFrame is a data.frame object containing our data
  
  # output: Produces a bar plot showing the average number of cases between 1970-2015
  # for the counties in dataFrame
  subDataFrame <- subset(dataFrame,dataFrame$counties != "Sweden")
  subDataFrame$counties <- factor(subDataFrame$counties)
  
  n <- length(levels(subDataFrame$counties))
  groups <- c(numeric(n))
  counties <- levels(subDataFrame$counties)
  
  for(i in 1:length(counties)){
    currentcounty <- subDataFrame[which(subDataFrame$counties == counties[i]),]
    totalCount <- sum(currentcounty$cases)
    average <- totalCount/nrow(currentcounty)
    groups[i] <- average
    #groups[i] <- median(currentcounty)
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
  m <- nrow(subDataFrame)
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
  n <- length(levels(dataFrameSubset$age))
  groups <- c(numeric(n))
  ages <- levels(dataFrameSubset$age)
  
  for(i in 1:length(ages)){
    currentGroup <- dataFrameSubset[which(dataFrameSubset$age == ages[i]),]
    currentCount <- median(currentGroup$cases)
    groups[i] <- currentCount
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
  maxVek <- c(numeric(n))
  minVek <- c(numeric(n))
  for(i in 1:n){
    regSpec <- dataFrame[which(dataFrame$counties == county & dataFrame$age == ageGroups[i]),]
    men <- regSpec[which(regSpec$sex == "Men"),]
    women <- regSpec[which(regSpec$sex == "Women"),]
    totalNumber <- men$cases + women$cases
    
    maxVek[i] <- max(totalNumber)
    minVek[i] <- min(totalNumber)
  }
  
  maxVal <- max(maxVek)
  minVal <- min(minVek)
  
  plot(0,0,xlim=c(1970,2015),ylim=c(minVal,maxVal),main = paste("Both sexes, county: ",county),
       xlab = "Years",ylab="Number of cases per 100 000 inhabitants")
  
  for(i in 1:n){
    regSpec <- dataFrame[which(dataFrame$counties == county & dataFrame$age == ageGroups[i]),]
    men <- regSpec[which(regSpec$sex == "Men"),]
    women <- regSpec[which(regSpec$sex == "Women"),]
    totalNumber <- men$cases + women$cases
    lines(yearsHere,rev(totalNumber),col=colVec[i])
  }
  legend('topleft',legend=ageGroups,col=colVec,pch=1,lty=c(1,1))
}

ageGroupComparisonTime(nonMelanomaRates,"Blekinge",c("0-4","50-54","80-84"),colorVector)

countyComparisonTime<-function(dataFrame,counties,ageGroup,colVec){
  # input: "dataFrame" is the data.frame we will be using, "counties" is a vector containing the
  # counties we will compare to one another, "ageGroup" is the age group we are interested in,
  # "sex" is the sex we are interested in and "colVec" is a vector of colors we will use to color
  # the lines
   
  # output: Produces a plot of lines, one plot for each county, between 1970 and 2015 for our given
  # age group and sex.
  yearsHere <- rev(unique((dataFrame$years)))
  n <- length(counties)
  maxVek <- c(numeric(n))
  minVek <- c(numeric(n))
  for(i in 1:n){
    regSpec <- dataFrame[which(dataFrame$counties == counties[i] & dataFrame$age == ageGroup),]
    men <- regSpec[which(regSpec$sex == "Men"),]
    women <- regSpec[which(regSpec$sex == "Women"),]
    totalNumber <- men$cases + women$cases
    
    maxVek[i] <- max(totalNumber)
    minVek[i] <- min(totalNumber)
  }
  
  maxVal <- max(maxVek)
  minVal <- min(minVek)
  
  plot(0,0,xlim=c(1970,2015),ylim=c(minVal,maxVal),main = paste("Both sexes, age group: ",ageGroup),
       xlab = "Years",ylab="Number of cases per 100 000 inhabitants")
  
  for(i in 1:n){
    regSpec <- dataFrame[which(dataFrame$counties == counties[i] & dataFrame$age == ageGroup),]
    men <- regSpec[which(regSpec$sex == "Men"),]
    women <- regSpec[which(regSpec$sex == "Women"),]
    totalNumber <- men$cases + women$cases
    lines(yearsHere,rev(totalNumber),col=colVec[i])
  }
  legend('topleft',legend=counties,col=colVec,pch=1,lty=c(1,1))
}

countyComparisonTime(nonMelanomaRates,c("Halland", "Stockholm", "Skåne"),"85+",colorVector)
countyComparisonTime(nonMelanomaRates,c("Blekinge", "Västerbotten", "Jönköping"),"70-74",colorVector)
countyComparisonTime(nonMelanomaRates,c("Halland","Blekinge"),"85+",colorVector)

# The grouping of counties is arbitrary and can be done in other ways, I tried grouping them "horizontally" in a
# geographic sense.
region1 <- c("Skåne","Blekinge")
region2 <- c("Kalmar","Kronoberg","Halland")
region3 <- c("Gotland","Jönköping","VästraGötaland","Östergötland")
region4 <- c("Södermanland","Örebro","Värmland")
region5 <- c("Stockholm","Västmanland","Uppsala")
region6 <- c("Dalarna","Gävleborg")
region7 <- c("Jämtland", "Västernorrland")
region8 <- c("Västerbotten","Norrbotten")
allRegions <- list(region1,region2,region3,region4,region5,region6,region7,regiony8)

# Plots for men
for(x in allcounties){
  countyComparisonTime()(nonMelanomaRates,x,"85+","Men",colorVector)
}

# Plots for women
for(x in allcounties){
  countyComparisonTime()(nonMelanomaRates,x,"85+","Women",colorVector)
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
    temp$cases <- sum(head(currentYear$cases,18))
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
  
  men <- dataFrame[which(dataFrame$sexInd == "Men" & dataFrame$counties == county & dataFrame$age == ageGroup),]
  women <- dataFrame[which(dataFrame$sexInd == "Women" & dataFrame$counties == county & dataFrame$age == ageGroup),]
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

sexComparison(nonMelanomaRates,"Halland","85+")
sexComparison(nonMelanomaRates,"Halland","45-49")

sexComparison(nonMelanomaRates,"Sweden","85+")
sexComparison(nonMelanomaRates,"Sweden","45-49")

sexComparison(nonMelanomaRates,"Skåne","85+")
sexComparison(nonMelanomaRates,"Skåne","45-49")

sexComparison(nonMelanomaRates,"VästraGötaland","85+")
sexComparison(nonMelanomaRates,"VästraGötaland","45-49")

for(x in regions){
  sexComparison(nonMelanomaRates,x,"85+")
}

for(x in regions){
  sexComparison(nonMelanomaRates,x,"80-84")
}

for(x in regions){
  sexComparison(nonMelanomaRates,x,"75-79")
}

for(x in regions){
  sexComparison(nonMelanomaRates,x,"70-74")
}

for(x in regions){
  sexComparison(nonMelanomaRates,x,"65-69")
}

for(x in regions){
  sexComparison(nonMelanomaRates,x,"60-64")
}

for(x in regions){
  sexComparison(nonMelanomaRates,x,"55-59")
}


for(x in regions){
  sexComparison(nonMelanomaRates,x,"50-54")
}
