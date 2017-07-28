library(MASS)
library(splines)
library(boot)
library(curl)

source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/dataFormating.R")

# The offset variable is formated and taken care of.
swedishPopulation <- read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/swedishPopulation.csv"),header=F,sep=";",encoding="UTF-8")
numericalValues <- swedishPopulation[4:length(swedishPopulation)]
numericalValues <- numericalValues[3:10,]
numericalValues <- data.frame(colSums(numericalValues))
colnames(numericalValues) <- c("Antal")
population <- numericalValues$Antal
population <- rev(population)

cancerDataCounts <- formatDF(read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/master/counts.csv"),
                                      header=F,sep=";",skip=2,encoding="UTF-8"))

# Have to do this since read.csv() orders everything alphabetically which you don't want
# There's probably a faster way to do this but I don't know about it at the moment.
lst<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
       "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
age <- rep(factor(lst,levels=lst),nrow(cancerDataCounts)/length(cancerDataCounts$age))
cancerDataCounts$age <- age

# Change the "sex" variable to an indicator variable with the value 1 for men and 0 for women
cancerDataCounts$sexInd <- gsub("Men",1,cancerDataCounts$sexInd)
cancerDataCounts$sexInd <- gsub("Women",0,cancerDataCounts$sexInd)
cancerDataCounts$sexInd <- as.numeric(cancerDataCounts$sexInd)

# "Sweden" is removed as a factor, "Stockholm" is chosen as refernence level for "counties"
cancerDataCounts <- subset(cancerDataCounts,counties != "Sweden")
cancerDataCounts$counties <- factor(cancerDataCounts$counties)
cancerDataCounts$counties <- relevel(cancerDataCounts$counties,ref="Stockholm")

# We use the youngest age group as reference level.
cancerDataCounts$age <- relevel(cancerDataCounts$age,ref="0-4")

# The variable "years" becomes "years after 1970" but we still call it "years"
cancerDataCounts$years <- cancerDataCounts$years - rep(mean(1970:2015),nrow(cancerDataCounts))

repeated10FoldCV<-function(model,dataFrame,k){
  # input: "model" is our model we wish to evaluate, "dataFrame" is the data.frame "model" is built on
  # and "k" is the number of times we repeat the 10-fold CV.
  
  # output: Returns the average value of our CV error estimate from k repetitions of 10-fold CV.
  set.seed(1)
  resVec <-c(numeric(k))
  for(i in 1:k){
    resVec[i] <- cv.glm(data=dataFrame,glmfit = model,K=10)$delta[2]
  }
  return(mean(resVec))
  # Later when we want to use the "one standard error rule" we can choose to return a vector with
  # mean(resVec) and sd(resVec) as well, sd(resVec) will be our standard error.
}

formatPopulationData<-function(populationVector,dataFrame){
  # input: "populationVector" is a vector of the population in Sweden during the years 1970-2015, dataFrame
  # is the data.frame for which we want to add the population as a variable.
  
  # output: Returns a vector containing the population of Sweden formatted so that it can be added to
  # our data.frame object of interest.
  
  # n = number of times we should replicate every element in populationVector so all the data points for a given
  # year are matched with the correct element in populationVector.
  n <- nrow(dataFrame[which(dataFrame$years == dataFrame$years[1]),])
  m <- nrow(dataFrame)
  resVec <- c()
  
  # Inefficient, could be made faster.
  for(i in 1:46){
    resVec <- append(resVec,rep(populationVector[i],n/2))
  }
  resVec <- rep(resVec,2)
  return(resVec)
}

cancerDataCounts$pop <- formatPopulationData(population,cancerDataCounts)

PoisMod <- glm(cases ~ years + sexInd + counties + age + offset(log(pop)),family=poisson ,data=cancerDataCounts)
NegBinMod <- glm.nb(cases ~ years + sexInd + counties + age + offset(log(pop)), data=cancerDataCounts)

diagnosticPlots <- function(model,dataFrame){
  # input: "model" is a model object which we want to examine, "dataFrame" is the data.frame object
  # "model" was built with.
  
  # output: Produces 3 plots for a given model: The deviance residuals vs the fitted response values,
  # the fitted values vs the actual values and a Cook's distance plot for our model.
  
  fitted <- model$fitted.values
  residuals <- residuals(model,type="deviance")
  actual <- dataFrame$cases
  
  plot(fitted,residuals,xlab="Fitted values",ylab="Deviance residuals",
       main = "Deviance residuals vs fitted values for model")
  plot(fitted,actual,xlab="Fitted values of response variable",ylab="Actual values of response variable",
       main = "Fitted vs actual values of response variable")
  plot(cooks.distance(model),main = "Cook's distance of model")
}

diagnosticPlots(PoisMod,cancerDataCounts)
diagnosticPlots(NegBinMod,cancerDataCounts)
# A Poisson model seems to be better than a negative binomial one. Interestingly enough, looking at the
# AIC-values gives a different picture since AIC(mod2) = 80356.17 and AIC(mod3) = 76891.58.

val1<-repeated10FoldCV(PoisMod,cancerDataCounts,5)
# val1 = 10.97736

val2<-repeated10FoldCV(NegBinMod,cancerDataCounts,5)
# Takes a lot of time to compute
# val2 = 12.75556

# A Poisson regression seems better than a negative binomial one, we use this as a springboard for further ideas.

testAgeGrouping <- function(dataFrame,ageGrouping){
  # input: dataFrame is the data.frame object we want to change, ageGrouping is a vector containing
  # the new age grouping we want to try out.
  
  # output: Returns a data frame with the new age grouping that was given as a parameter.
  
  age <- rep(as.factor(ageGrouping),length(dataFrame$age)/18)
  df <- data.frame(dataFrame$cases,dataFrame$years,dataFrame$sexInd,dataFrame$counties,age,dataFrame$pop)
  colnames(df) <- c("cases","years","sexInd","counties","age","pop")
  return(df)
}

testModelAgeGrouping <- function(lst,dataFrame){
  # input: "lst" is a vector containing the new age grouping we want to try, dataFrame is the data.frame
  # whose factor levels for "age" we want to change.
  
  # output: Returns the average value of a CV error estimate from 10-fold CV repeated 5 times when using
  # a Poisson regression model only containing main effects when we use the changed data.frame object.
  
  df <- testAgeGrouping(dataFrame,lst)
  mod <- glm(cases ~ years + sexInd + counties + age + offset(log(pop)),family = poisson, data = df)
  repeatedCV <- repeated10FoldCV(mod,df,5)
  return(repeatedCV)
}

plotAgeGroupings <- function(ageGroupings,names,dataFrame){
  # input: ageGroupings is a list of vectors where each vector is an age group coding, "names" is the
  # name for every such vector and "dataFrame" is the data.frame object whose levels of the variable
  # "age" we want to change.
  
  # output: Returns a plot of the CV-error estimates given by testModelAgeGrouping for the different
  # age groupings in ageGroupings so we can see which one is best.
  
  resVec <- c(numeric(length(ageGroupings)))
  for(i in 1:length(resVec)){
    resVec[i] <- testModelAgeGrouping(unlist(ageGroupings[i]),dataFrame)
  }
  plot(resVec,xaxt="n",ylab="CV error estimate",xlab="Age groups",main="CV error estimates and age groups",type='n')
  axis(1,at=seq_along(resVec),labels=as.character(names),xlab="Age groupings")
  box()
  text(c(1:length(names)),resVec,labels=round(resVec,5),cex=0.7)
}

# Ideas for age groupings come from the plots of the average values in different age groups.
lst1<-c("0-9","0-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
lst2<-c("0-14","0-14","0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
lst3<-c("0-19","0-19","0-19","0-19","20-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
lst4<-c("0-24","0-24","0-24","0-24","0-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
lst5<-c("0-29","0-29","0-29","0-29","0-29","0-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
lst6<-c("0-9","0-9","10-19","10-19","20-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
lst7<-c("0-9","0-9","10-14","15-24","15-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")

plotAgeGroupings(list(lst1,lst2,lst3,lst4,lst5,lst6,lst7),
                 c("0-9 years","0-14 years", "0-19 years", "0-24 years",
                   "0-29 years","0-9,10-19","0-9,15-24"),cancerDataCounts)
# lst1 seems best, gives CV error estimate = 10.97735

# Current model:
df3 <- testAgeGrouping(cancerDataCounts,lst1)
modtest1 <- glm(cases ~ years + sexInd + counties + age + offset(log(pop)),family=poisson, data = df3)
# repeated10FoldCV(modtest1,df3,5) = CV error estimate = 10.97735

testCountyGrouping <- function(dataFrame,countyGrouping){
  # input: "countyGrouping" is list containing 2 vectors of the same length, one containing the names
  # of counties we wish to collapse as one factor level and the other one containing the name for the new
  # level. The new name is matched with the old one. "dataFrame" is the data.frame object whose factor
  # levels for the variable "counties" we want to change.
  
  # output: Returns a new data.frame object with changed factor levels for the variable "counties"
  
  oldNames <- countyGrouping[1]
  newNames <- countyGrouping[2]
  
  df <- data.frame(dataFrame$cases,dataFrame$sexInd,dataFrame$years,dataFrame$age,dataFrame$counties,dataFrame$pop)
  colnames(df) <- c("cases","sexInd","years","age","counties","pop")
  n <- length(unlist(list(oldNames)))
  
  for(i in 1:n){
    levels(df$counties)[levels(df$counties) == unlist(list(oldNames))[i]] <- unlist(list(newNames))[i]
  }
  return(df)
}

# Ideas for possible groupings are given by the plot comparing counties. The idea is to group counties
# with approximately the same average number of cases that are geographically close. Possible groupings
# are given below the functions.

testCountyGroupingModel<-function(grouping,dataFrame){
  # input: "grouping" is a vector containing the names of counties we want to collapse to one factor
  # level of the variable "counties" in dataFrame, which is the data.frame object we want to change.
  
  # output: Returns the CV error estimate repeated 10-fold CV (repeated 5 times) for the Poisson
  # regression model containing only the main effects using the changed data.frame object mentioned
  # above.
  
  newReg <- rep("reg1",length(grouping))
  df <- testCountyGrouping(dataFrame,list(grouping,newReg))
  model <- glm(cases ~ years + age + counties + sexInd + offset(log(pop)),family=poisson,data=df)
  repeatedCV <- repeated10FoldCV(model,df,5)
  return(repeatedCV)
}

plotCountyGroupings<-function(countyGroupings,names,dataFrame){
  # input: countyGroupings is a list of vectors where each vector contains county levels we want to
  # collapse, "names" is the name for every such vector and "dataFrame" is the data.frame object whose levels of the variable
  # levels of the variable "counties" we want to change.
  
  # output: Returns a plot of the CV-error estimates given by testModelAgeGrouping for the different
  # age groupings in ageGroupings so we can see which one is best.
  resVec <- c(numeric(length(countyGroupings)))
  for(i in 1:length(resVec)){
    resVec[i] <- testCountyGroupingModel(unlist(countyGroupings[i]),dataFrame)
  }
  plot(resVec,xaxt="n",ylab="CV error estimate",main="County groupings and CV error estimates",type='n')
  axis(1,at=seq_along(resVec),labels=as.character(names),xlab="County groupings",cex.axis=0.7)
  box()
  text(c(1:length(names)),resVec,labels=round(resVec,5),cex=0.7)
}

grp1 <- c("Skåne","Blekinge")
grp2 <- c("VästraGötaland","Jönköping")
grp3 <- c("Dalarna","Gävleborg")
grp4 <- c("Jämtland","Norrbotten","Västerbotten","Västernorrland")
grp5 <- c("Södermanland","Örebro")
grp6 <- c("Södermanland","Örebro","Uppsala")
grp7 <- c("Gotland","Kalmar")
grp8 <- c("Gotland","Kalmar","Kronoberg")
grp9 <- c("Gotland","Kalmar","Kronoberg","Östergötland")

plotCountyGroupings(list(grp1,grp2,grp3,grp4,grp5,grp6,grp7,grp8,grp9),
                    c("Sk-Bl","Vä-Jö","Da-Gä","Jä-No-VB-VN",
                      "Sö-Ör","Sö-Ör-Up","Go-Ka","Go-Ka-Kr",
                      "Go-Ka-Kr-Ös"),df3)

# We collapse "Dalarna" and "Gävleborg".

# We try polynomial regression. In case a polynomial with a very high degree seems optimal we need
# to remember that overfitting might be occurring and that we might observe Runges phenomenon.
polynomialRegressionCV<-function(n,dataFrame){
  # input: n = degree of polynomial we want to try for the variable "years" in a Poisson regression
  # model only containing the main effects. dataFrame is the data.frame object we use for building the
  # model.
  
  # output: Returns the CV error estimate for the model containing a polynomial of "years" with degree n 
  
  mod <- glm(cases ~ poly(years,degree = n) + sexInd + counties + age + offset(log(pop)),family=poisson,data = dataFrame)
  environment(repeated10FoldCV) <- environment()
  repeatedCV <- repeated10FoldCV(mod,dataFrame,5)
  return(repeatedCV)
}

plotPolynomialCV<-function(degrees,dataFrame){
  # input: degress = the maximum degree of our polynomial we want to plot. dataFrame is the data.frame
  # object we use to build our regression model in polynomialRegressionCV.
  
  # output: Produces a plot showing the CV error estimates for regression models with different degrees
  # of the polynomial of "years" so we can compare them.
  res <- c(numeric(degrees))
  x <- 1:degrees
  for(i in 1:degrees){
    val<-polynomialRegressionCV(i,dataFrame)
    res[i] <- val
  }
  
  plot(res,xaxt="n",xlab="Degree of polynomial",ylab="CV error estimate",main="Polynomial degrees and CV error estimates",type='n')
  axis(1,at=seq_along(res),labels=as.character(x),xlab="Degrees of polynomial",cex.axis=0.7)
  box()
  text(x,res,labels=round(res,5),cex=0.7)
}

plotPolynomialCV(20,df3)
# Takes a while to run. A polynomial of degree 6 seems best, gives a CV error estimate of 10.73422

# A spline function can be used instead of polynomial regression. Functions for this are defined below.

# We can use spline functions with multiple knots
splineModelDF <- function(dataFrame,n){
  # input: dataFrame is the data.frame object we use to build our Poisson regression model containing
  # the cubic spline function of years with "n" degrees of freedom and otherwise only main effects.
  
  # output: Returns the CV error estimate of the model described above.
  environment(repeated10FoldCV) <- environment()
  mod <- glm(cases ~ bs(years,n) + sexInd + age + counties + offset(log(pop)),family=poisson,data=dataFrame)
  repeatedCV <- repeated10FoldCV(mod,dataFrame,5)
  return(repeatedCV)
}

plotSplineModels<-function(dataFrame,n){
  # n = maximum number of degrees of freedom tried, must be greater than or equal to 3
  # input: n = maximum number of degrees of freedom in our cubic spline function for "years" we want
  # to try (n must be greater than or equal to 3), dataFrame = data.frame object used to build our 
  # regression model with splineModelDF.
  
  # output: Produces a plot showing the CV error estimates of models with different degrees of freedom
  # so we can compare them.
  
  resVec <- c(numeric(n-2))
  knotVec <- c(3:n)
  for(i in 1:(n-2)){
    resVec[i] <- splineModelDF(dataFrame,knotVec[i])
  }
  
  plot(resVec,xaxt="n",xlab="Degrees of freedom in cubic spline",ylab="CV error estimate",main="Degrees of freedom in cubic spline and CV error estimates",type = 'n')
  axis(1,at=seq_along(resVec),labels=knotVec,xlab="Degrees of freedom in cubic spline",cex.axis=0.7)
  box()
  text(c(1:length(knotVec)),resVec,labels=round(resVec,5),cex=0.7)
}

plotSplineModels(df3,10)
# Shows that df = 6 is the best choice

# The best model so far:
splineMod7 <- glm(cases ~ bs(years,df=6) + sexInd + age + counties + offset(log(pop)),family=poisson,data=df3)
# repeated10FoldCV(splineMod7,df3,5) = 10.72815

# Try adding interaction terms

splineMod14 <- glm(cases ~ bs(years,df=6) + sexInd + age + counties + sexInd:age +offset(log(pop)),family=poisson,data=df3)
splineVal14 <- repeated10FoldCV(splineMod14,df3,5)
# splineVal14 = 9.301133

splineMod15 <- glm(cases ~ bs(years,df=6) + sexInd + age + counties + sexInd:age + sexInd:counties +offset(log(pop)),family=poisson,data=df3)
splineVal15 <- repeated10FoldCV(splineMod15,df3,5)
# splineVal15 = 9.214266

splineMod16 <- glm(cases ~ bs(years,df=6) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:bs(years,df=6)  +offset(log(pop)),family=poisson,data=df3)
splineVal16 <- repeated10FoldCV(splineMod16,df3,5)
# splineVal16 = 9.09313

splineMod17 <- glm(cases ~ bs(years,df=6) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:bs(years,df=6) + age:counties + offset(log(pop)),family=poisson,data=df3)
splineVal17 <- repeated10FoldCV(splineMod17,df3,5)
# Takes a very long time to compute
# splineVal17 = 9.336958

splineMod18 <- glm(cases ~ bs(years,df=6) + sexInd + age + counties + 
                     sexInd:age + sexInd:counties + sexInd:bs(years,df=6) + 
                     age:counties +  age:bs(years,df=6)
                   + offset(log(pop)),family=poisson,data=df3)
# fitted rates numerically 0 occurred, this model is ignored

splineMod19 <- glm(cases ~ bs(years,df=6) + sexInd + age + counties + 
                     sexInd:age + sexInd:counties + sexInd:bs(years,df=6) + 
                     age:counties +  age:bs(years,df=6)
                   + counties:bs(years,df=6)
                   + offset(log(pop)),family=poisson,data=df3)
# fitted rates numerically 0 occurred, this model is ignored

splineMod20 <- glm(cases ~ bs(years,df=6) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:bs(years,df=6) + age:counties + sexInd:age:counties +offset(log(pop)),family=poisson,data=df3)
splineVal20 <- repeated10FoldCV(splineMod20,df3,5)
# splineVal20 = 9.482725
# Took almost the entire day to compute.

# Used a hard coded plot for these models to make it 
splineValues <- c(splineVal14,splineVal15,splineVal16,splineVal17,9.482725)
interactionTerms <- c("sexInd:age",
                      "+sexInd:counties",
                      "+sexInd:bs(years,df=6)",
                      "+age:counties",
                      "+sexInd:age:counties")
# Everything after "sexInd:age" tells us what we added to the previous one. I.e. 
# "+sexInd:counties" means the model with sexInd:age + sexInd:counties and "+sexInd:bs(years,df=6)"
# means the model with sexInd:age + sexInd:counties + sexInd:bs(years,df=6)
# The models plotted above have increasing complexity, i.e. more terms are added

plot(splineValues,xaxt='n',main="Added interaction terms and CV error estimates",
     ylab="CV error estimate",type='n')
legend("topleft",legend=interactionTerms,fill=c("blue","red","green","black","purple"),ncol=1,cex=0.7,xpd=TRUE)
box()
text(c(1:length(splineValues)),splineValues,labels=round(splineValues,5),
     col=c("blue","red","green","black","purple"),cex=0.7)

# splineMod16 seems to be the best of these so we continue on from that model.

#Try removing variables from splineMod7
plotRegressionModels <- function(dataFrame,formulaVec){
  # input: dataFrame is the data.frame object used to build our regression models, formulaVec is a vector
  # of strings which are the formulas used in our regression models.
  
  # output: Produces a plot of the CV error estimates for our different models so we can compare them.
  
  resVec <- c(numeric(length(formulaVec)))
  environment(repeated10FoldCV) <- environment()
  for(i in 1:length(resVec)){
    form <- as.formula(formulaVec[i])
    mod <- glm(form,family=poisson,data=dataFrame)
    resVec[i] <- repeated10FoldCV(mod,dataFrame,5)
  }
  plot(resVec,xaxt='n',main="Removed variables and CV error estimates",ylab="CV error estimate",type='n')
  legend("topleft",legend=formulaVec,fill=c("blue","red","green","black","purple"),ncol=1,cex=0.7,xpd=TRUE)
  text(c(1:length(formulaVec)),resVec,labels=round(resVec,5),col=c("blue","red","green","black","purple"),cex=0.7)
}

str1 <- "cases ~ bs(years,df=6) + age + sexInd + counties + offset(log(pop))"
str2 <- "cases ~ sexInd + age + counties + offset(log(pop))"
str3 <- "cases ~ bs(years,df=6) + age + counties + offset(log(pop))"
str4 <- "cases ~ bs(years,df=6) + sexInd + age+ offset(log(pop))"
str5 <- "cases ~ bs(years,df=6) + sexInd + counties + offset(log(pop))"
plotRegressionModels(df3,c(str1,str2,str3,str4,str5))


# We choose splineMod16 as the best model among the considered ones and take a
# look at the diagnostic plots below.
diagnosticPlots(splineMod16,df3)
