install.packages("MASS")
install.packages("splines")
install.packages("boot")
install.packages("curl")
install.packages("Hmisc")
library(Hmisc)
library(MASS)
library(splines)
library(boot)
library(curl)
source("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/Revised/dataFormating.R")

# The offset variable is formated and taken care of.
swedishPopulation = read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/Revised/swedishPopulation.csv"),header=F,sep=";",encoding="UTF-8")
numericalValues = swedishPopulation[4:length(swedishPopulation)]
numericalValues = numericalValues[3:10,]
numericalValues = data.frame(colSums(numericalValues))
colnames(numericalValues) = c("Antal")
population = numericalValues$Antal
population = rev(population)

cancerDataCounts = formatDF(read.csv(curl("https://raw.githubusercontent.com/SebastianAlexanderBergstrom/nonMelanomaSkinCancerSweden/Revised/counts.csv"),
                                      header=F,sep=";",skip=2,encoding="UTF-8"))

# Change the "sex" variable to an indicator variable with the value 1 for men and 0 for women
cancerDataCounts$sexInd = ifelse(cancerDataCounts$sex == "Men",1,0)

# "Sweden" is removed as a factor, "Stockholm" is chosen as refernence level for "counties"
cancerDataCounts = subset(cancerDataCounts,counties != "Sweden")
cancerDataCounts$counties = factor(cancerDataCounts$counties)
cancerDataCounts$counties = relevel(cancerDataCounts$counties,ref="Stockholm")

# We use the youngest age group as reference level.
cancerDataCounts$age = relevel(cancerDataCounts$age,ref="0-4")

# We center the variable "years"
cancerDataCounts$years = cancerDataCounts$years - rep(mean(1970:2015),nrow(cancerDataCounts))

repeated10FoldCV=function(model,dataFrame,k){
  # input: "model" is our model we wish to evaluate, "dataFrame" is the data.frame "model" is built on
  # and "k" is the number of times we repeat the 10-fold CV.
  
  # output: Returns the average value of our CV error estimate from k repetitions of 10-fold CV as well as the
  # standard error of the CV error estimate.
  set.seed(1)
  resVec =c(numeric(k))
  for(i in 1:k){
    resVec[i] = cv.glm(data=dataFrame,glmfit = model,K=10)$delta[2]
  }
  return(list(cvEstimate = mean(resVec),seEstimate = sd(resVec)))
}

formatPopulationData=function(populationVector,dataFrame){
  # input: "populationVector" is a vector of the population in Sweden during the years 1970-2015, dataFrame
  # is the data.frame for which we want to add the population as a variable.
  
  # output: Returns a vector containing the population of Sweden formatted so that it can be added to
  # our data.frame object of interest.
  
  # n = number of times we should replicate every element in populationVector so all the data points for a given
  # year are matched with the correct element in populationVector.
  n = nrow(dataFrame[which(dataFrame$years == dataFrame$years[1]),])
  resVec = c()
  
  numberOfYears = length(unique(dataFrame$years))
  for(i in 1:numberOfYears){
    resVec = append(resVec,rep(populationVector[i],n/2))
  }
  resVec = rep(resVec,2)
  return(resVec)
}

cancerDataCounts$pop = formatPopulationData(population,cancerDataCounts)

PoisMod = glm(cases ~ years + sexInd + counties + age + offset(log(pop)),family=poisson ,data=cancerDataCounts)
NegBinMod = glm.nb(cases ~ years + sexInd + counties + age + offset(log(pop)), data=cancerDataCounts)

diagnosticPlots = function(model,dataFrame){
  # input: "model" is a model object which we want to examine, "dataFrame" is the data.frame object
  # "model" was built with.
  
  # output: Produces 3 plots for a given model: The deviance residuals vs the fitted response values,
  # the fitted values vs the actual values and a Cook's distance plot for our model.
  
  fitted = model$fitted.values
  devianceResiduals = residuals(model,type="deviance")
  actual = dataFrame$cases
  
  plot(fitted,devianceResiduals,xlab="Fitted values",ylab="Deviance residuals",
       main = "Deviance residuals vs fitted values for model")
  plot(fitted,actual,xlab="Fitted values of response variable",ylab="Actual values of response variable",
       main = "Fitted vs actual values of response variable")
  plot(cooks.distance(model),main = "Cook's distance of model")
}

diagnosticPlots(PoisMod,cancerDataCounts)
diagnosticPlots(NegBinMod,cancerDataCounts)
# A Poisson model seems to be better than a negative binomial one. Interestingly enough, looking at the
# AIC-values gives a different picture since AIC(mod2) = 80356.17 and AIC(mod3) = 76891.58.

# Compute cross validation errors for Poisson and negative binomial, this takes a while.
cat("Poisson, CV error etstimate:",repeated10FoldCV(PoisMod,cancerDataCounts,5)$cvEstimate,
    "Standard error estimate: ",repeated10FoldCV(PoisMod,cancerDataCounts,5)$seEstimate,"\nNegative binomial, CV error estimate:",
    repeated10FoldCV(NegBinMod,cancerDataCounts,5)$cvEstimate, "Standard error estimate: ",
    repeated10FoldCV(NegBinMod,cancerDataCounts,5)$seEstimate)
#Poisson, CV error etstimate: 10.97736 Standard error estimate:  0.01831321 
#Negative binomial, CV error estimate: 12.75556 Standard error estimate:  0.008404553
# A Poisson regression seems better than a negative binomial one, we use this as a springboard for further ideas.

testAgeGrouping = function(dataFrame,ageGrouping){
  # input: dataFrame is the data.frame object we want to change, ageGrouping is a vector containing
  # the new age grouping we want to try out.
  
  # output: Returns a data frame with the new age grouping that was given as a parameter.
  
  # Change "18" so it isn't hard coded
  # Think "18" should be replaced by length(unique(dataFrame$age))
  age = rep(as.factor(ageGrouping),length(dataFrame$age)/length(unique(dataFrame$age)))
  newDataFrame = data.frame(dataFrame$cases,dataFrame$years,dataFrame$sexInd,dataFrame$counties,age,dataFrame$pop)
  colnames(newDataFrame) = c("cases","years","sexInd","counties","age","pop")
  return(newDataFrame)
}

# NEED TO ADD ERROR BARS IN testModelAgeGrouping and plotAgeGroupings
testModelAgeGrouping = function(lst,dataFrame){
  # input: "lst" is a vector containing the new age grouping we want to try, dataFrame is the data.frame
  # whose factor levels for "age" we want to change.
  
  # output: Returns the average value of a CV error estimate from 10-fold CV repeated 5 times when using
  # a Poisson regression model only containing main effects when we use the changed data.frame object.
  
  newDataFrame = testAgeGrouping(dataFrame,lst)
  mod = glm(cases ~ years + sexInd + counties + age + offset(log(pop)),family = poisson, data = newDataFrame)
  repeatedCV = repeated10FoldCV(mod,newDataFrame,5)
  return(repeatedCV)
}

plotAgeGroupings = function(ageGroupings,names,dataFrame){
  # input: ageGroupings is a list of vectors where each vector is an age group coding, "names" is the
  # name for every such vector and "dataFrame" is the data.frame object whose levels of the variable
  # "age" we want to change.
  
  # output: Returns a plot of the CV-error estimates given by testModelAgeGrouping for the different
  # age groupings in ageGroupings so we can see which one is best.
  
  # Might be possible to use vector operations here instead, maybe something along the lines of 
  # cvEstimates = testModelAgeGrouping(unlist(ageGroupings),dataFrame)$cvEstimate
  
  cvEstimates = c(numeric(length(ageGroupings)))
  seEstimates = c(numeric(length(ageGroupings)))
  for(i in 1:length(cvEstimates)){
    values = testModelAgeGrouping(unlist(ageGroupings[i]),dataFrame)
    cvEstimates[i] = values$cvEstimate
    seEstimates[i] = values$cvEstimate
  }
  
  minimumValue = min(cvEstimates-2*seEstimates)
  maximumValue = max(cvEstimates+2*seEstimates)
  
  plot(c(1:length(ageGroupings)),cvEstimates,type="n",xaxt="n",main="CV error estimates and age groupings",
       xlab = "Age groupings",ylim=c(minimumValue,maximumValue),ylab="CV error estimates")
  errbar(c(1:length(ageGroupings)),cvEstimates,cvEstimates+seEstimates,cvEstimates-seEstimates,
         xaxt="n",main="CV error estimates and county groupings",ylab="CV error estimates",xlab="Age groupings",add=T)
  axis(1,at=seq_along(cvEstimates),labels=as.character(names),xlab="Age groupings")
}

# Can also try collapsing more age groups. 
# Can probably use rep() here instead
ageGrp1=c("0-9","0-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp2=c("0-14","0-14","0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp3=c("0-19","0-19","0-19","0-19","20-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp4=c("0-24","0-24","0-24","0-24","0-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp5=c("0-29","0-29","0-29","0-29","0-29","0-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp6=c("0-9","0-9","10-19","10-19","20-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp7=c("0-9","0-9","10-14","15-24","15-24","25-29","30-34","35-39","40-44","45-49",
        "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")

# More age groups that can be tried. Based on the histogram it seems groups up to about 40-55 can be collapsed into
# one level. You'll try these for yourself and see if you arrive at anything. It's a bit inconsistent since you didn't
# do this previously so the comparison might be unfair but unless you notice a large discrepancy it shouldn't be a problem.
ageGrp8 = c("0-34","0-34","0-34","0-34","0-34","0-34","0-34","35-39","40-44","45-49",
             "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp9 = c("0-39","0-39","0-39","0-39","0-39","0-39","0-39","0-39","40-44","45-49",
             "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp10 = c("0-44","0-44","0-44","0-44","0-44","0-44","0-44","0-44","0-44","45-49",
             "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")
ageGrp11 = c("0-49","0-49","0-49","0-49","0-49","0-49","0-49","0-49","0-49","0-49",
              "50-54","55-59","60-64","65-69","70-74","75-59","80-84","85+")

plotAgeGroupings(list(ageGrp1,ageGrp2,ageGrp3,ageGrp4,ageGrp5,ageGrp6,ageGrp7),
                 c("0-9 years","0-14 years", "0-19 years", "0-24 years",
                   "0-29 years","0-9,10-19","0-9,15-24"),cancerDataCounts)
# Seems like we can collapse everything from 0-4 up to 25-29.

# Trying the new age groupings makes you a bit inconsistent since you didn't try it in the previous installment
# but in your defence you could see a clearly increasing relationship then which is ignored now with the 1-SE rule. 
# So you'll go ahead with trying the new age groupings as well. Inspirations come from the histogram, just as before.
# Try with the new age groupings as well
# We have to remember PoisMod at this point, since it might turn out that none of these age groups are better than that one.
# PoisMod, CV error etstimate: 10.97736 Standard error estimate:  0.01831321

# Need the standard error when using ageGrp1
dfAgeGrp1 = testAgeGrouping(cancerDataCounts,ageGrp1)
currentMod = glm(cases ~ years + sexInd + counties + age + offset(log(pop)),family=poisson, data = dfAgeGrp1)
currentVal = repeated10FoldCV(currentMod,dfAgeGrp1,5)
cat("Current model's CV error rate: ",currentVal$cvEstimate, ", current models standard error estimate: ",
    currentVal$seEstimate)
# CV error estimate = 10.97735, standard error estimate = 0.01831312
# This means we are willing to continue with models whose CV error estimate are between 10.95904 and 10.99567. We only
# actually care about the upper limit in our case.

# Using ageGrp1 gives the CV error estimate 10.97735, making it the best group. This means we'll compare CV error
# estimates of other models compared to this one from this point on.

plotAgeGroupings(list(ageGrp1,ageGrp2,ageGrp3,ageGrp4,ageGrp5,ageGrp8,ageGrp9,ageGrp10,ageGrp11),
                 c("0-9 years","0-14 years", "0-19 years", "0-24 years",
                   "0-29 years","0-34 years","0-39 years","0-44 years","0-49 years"),cancerDataCounts)
# Gives a different viewpoint, now it seems you can safely collapse 0-4 up to 35-39, which corresponds to ageGrp9 



# Change the names here
# Current model we'll continue from. However, when comparing models using the 1 SE-rule we'll still have to remember
# "currentMod" until we find a better one.
df3 = testAgeGrouping(cancerDataCounts,ageGrp9)
modtest1 = glm(cases ~ years + sexInd + counties + age + offset(log(pop)),family=poisson, data = df3)
val1 = repeated10FoldCV(modtest1,df3,5)
cat("Current model's CV error rate: ",val1$cvEstimate, ", current models standard error estimate: ",val1$seEstimate)
# CV error estimate = 10.98416, standard error estimate = 0.01805421

testCountyGrouping = function(dataFrame,countyGrouping){
  # input: "countyGrouping" is list containing 2 vectors of the same length, one containing the names
  # of counties we wish to collapse as one factor level and the other one containing the name for the new
  # level. The new name is matched with the old one. "dataFrame" is the data.frame object whose factor
  # levels for the variable "counties" we want to change.
  
  # output: Returns a new data.frame object with changed factor levels for the variable "counties"
  
  oldNames = countyGrouping[1]
  newNames = countyGrouping[2]
  
  df = data.frame(dataFrame$cases,dataFrame$sexInd,dataFrame$years,dataFrame$age,dataFrame$counties,dataFrame$pop)
  colnames(df) = c("cases","sexInd","years","age","counties","pop")
  n = length(unlist(list(oldNames)))
  
  for(i in 1:n){
    levels(df$counties)[levels(df$counties) == unlist(list(oldNames))[i]] = unlist(list(newNames))[i]
  }
  return(df)
}

# Ideas for possible groupings are given by the plot comparing counties. The idea is to group counties
# with approximately the same average number of cases that are geographically close. Possible groupings
# are given below the functions.

testCountyGroupingModel=function(grouping,dataFrame){
  # input: "grouping" is a vector containing the names of counties we want to collapse to one factor
  # level of the variable "counties" in dataFrame, which is the data.frame object we want to change.
  
  # output: Returns the CV error estimate repeated 10-fold CV (repeated 5 times) for the Poisson
  # regression model containing only the main effects using the changed data.frame object mentioned
  # above.
  
  newReg = rep("reg1",length(grouping))
  newDataFrame = testCountyGrouping(dataFrame,list(grouping,newReg))
  model = glm(cases ~ years + age + counties + sexInd + offset(log(pop)),family=poisson,data=newDataFrame)
  repeatedCV = repeated10FoldCV(model,newDataFrame,5)
  return(repeatedCV)
}

plotCountyGroupings=function(countyGroupings,names,dataFrame){
  # input: countyGroupings is a list of vectors where each vector contains county levels we want to
  # collapse, "names" is the name for every such vector and "dataFrame" is the data.frame object whose levels of the variable
  # levels of the variable "counties" we want to change.
  
  # output: Returns a plot of the CV-error estimates given by testModelAgeGrouping for the different
  # age groupings in ageGroupings so we can see which one is best.
  
  cvEstimates = c(numeric(length(countyGroupings)))
  seEstimates = c(numeric(length(countyGroupings)))
  for(i in 1:length(cvEstimates)){
    values = testCountyGroupingModel(unlist(countyGroupings[i]),dataFrame)
    cvEstimates[i] = values$cvEstimate
    seEstimates[i] = values$seEstimate
  }
  minimumValue = min(cvEstimates-2*seEstimates)
  maximumValue = max(cvEstimates+2*seEstimates)
  
  plot(c(1:length(countyGroupings)),cvEstimates,type="n",xaxt="n",main="CV error estimates and county groupings",
       xlab = "County groupings",ylim=c(minimumValue,maximumValue),ylab="CV error estimates")
  errbar(c(1:length(countyGroupings)),cvEstimates,cvEstimates+seEstimates,cvEstimates-seEstimates,
         xaxt="n",main="CV error estimates and county groupings",ylab="CV error estimates",xlab="County groupings",add=T)
  axis(1,at=seq_along(cvEstimates),labels=as.character(names),xlab="County groupings")
}

grp1 = c("Skåne","Blekinge")
grp2 = c("VästraGötaland","Jönköping")
grp3 = c("Dalarna","Gävleborg")
grp4 = c("Jämtland","Norrbotten","Västerbotten","Västernorrland")
grp5 = c("Södermanland","Örebro")
grp6 = c("Södermanland","Örebro","Uppsala")
grp7 = c("Gotland","Kalmar")
grp8 = c("Gotland","Kalmar","Kronoberg")
grp9 = c("Gotland","Kalmar","Kronoberg","Östergötland")

plotCountyGroupings(list(grp1,grp2,grp3,grp4,grp5,grp6,grp7,grp8,grp9),
                    c("Sk-Bl","Vä-Jö","Da-Gä","Jä-No-VB-VN",
                      "Sö-Ör","Sö-Ör-Up","Go-Ka","Go-Ka-Kr",
                      "Go-Ka-Kr-Ös"),df3)
# Becomes hard to interpret since the standard errors are small and some county groupings give much higher
# error rates. It's clear from the plot that you should only consider "Vä-Jö", "Da-Gä", "Jä-No-VB-VN", "Sö-Ör"
# and "Sö-Ör-Upp".
plotCountyGroupings(list(grp2,grp3,grp4,grp5,grp6),
                    c("Vä-Jö","Da-Gä","Jä-No-VB-VN",
                      "Sö-Ör","Sö-Ör-Up"),df3)
# Based on this, we decide to only collapse "Dalarna" and "Gävleborg". Now we'll see if it's significantly better than 
# currentMod.

# We collapse "Dalarna" and "Gävleborg".
df4 = testCountyGrouping(df3,c("Dalarna","Gävleborg"))
modtest2 = glm(cases ~ years + sexInd + counties + age + offset(log(pop)),family=poisson, data = df4)
val2 = repeated10FoldCV(modtest2,df4,5)
cat("Current model's CV error rate: ",val2$cvEstimate, ", current models standard error estimate: ",val2$seEstimate)
# CV error rate: 10.98305, standard error estimate: 0.01820177, which is within one standard error of the CV estimate
# for "currentModel", so we'll continue with this for now.

# We try polynomial regression. In case a polynomial with a very high degree seems optimal we need
# to remember that overfitting might be occurring and that we might observe Runges phenomenon.
polynomialRegressionCV=function(n,dataFrame){
  # input: n = degree of polynomial we want to try for the variable "years" in a Poisson regression
  # model only containing the main effects. dataFrame is the data.frame object we use for building the
  # model.
  
  # output: Returns the CV error estimate for the model containing a polynomial of "years" with degree n 
  
  mod = glm(cases ~ poly(years,degree = n) + sexInd + counties + age + offset(log(pop)),family=poisson,data = dataFrame)
  environment(repeated10FoldCV) = environment()
  repeatedCV = repeated10FoldCV(mod,dataFrame,5)
  return(repeatedCV)
}

plotPolynomialCV=function(degrees,dataFrame){
  # input: degress = the maximum degree of our polynomial we want to plot. dataFrame is the data.frame
  # object we use to build our regression model in polynomialRegressionCV.
  
  # output: Produces a plot showing the CV error estimates for regression models with different degrees
  # of the polynomial of "years" so we can compare them.
  
  x = 1:degrees
  
  cvEstimates = c(numeric(length(degrees)))
  seEstimates = c(numeric(length(degrees)))
  for(i in 1:degrees){
    values = polynomialRegressionCV(i,dataFrame)
    cvEstimates[i] = values$cvEstimate
    seEstimates[i] = values$seEstimate
  }
  
  minimumValue = min(cvEstimates-2*seEstimates)
  maximumValue = max(cvEstimates+2*seEstimates)
  
  plot(x,cvEstimates,type="n",xaxt="n",main="CV error estimates and degrees in polynomial function of the variable 'years' ",
       xlab = "Degree of polynomial",ylim=c(minimumValue,maximumValue),ylab="CV error estimates")
  errbar(x,cvEstimates,cvEstimates+seEstimates,cvEstimates-seEstimates,
         xaxt="n",main="CV error estimates and degrees of polynomial",ylab="CV error estimates",xlab="Degrees of polynomial",add=T)
  axis(1,at=seq_along(cvEstimates),labels=as.character(x),xlab="Degrees of polynomials")
}

plotPolynomialCV(20,df4)
# From the plot we conclude that using a polynomial of degree 5 seems to be
# the best option. Using a polynomial with such a high degree seems a bit surprising, but it's still
# slightly lower than when the 1-SE rule was not used. It's clearly better than the old "currentMod", so
# this will be the new model we compare others to.

currentMod = glm(cases ~ poly(years,5) + sexInd + counties + age + offset(log(pop)),family=poisson, data = df4)
currentVal = repeated10FoldCV(currentMod,df4,5)
cat("Current model's CV error rate: ",currentVal$cvEstimate, ", current models standard error estimate: ",
    currentVal$seEstimate)
# CV error estimate = 10.74241, standard error estimate = 0.01688076
# A bit surprising that the standard error estimate was slightly lower than before.
# This means we are willing to continue with models whose CV error estimate are between 10.72553 and 10.75929. We only
# actually care about the upper limit in our case.


# A spline function can be used instead of polynomial regression. Functions for this are defined below.
splineModelDF = function(dataFrame,n){
  # input: dataFrame is the data.frame object we use to build our Poisson regression model containing
  # the cubic spline function of years with "n" degrees of freedom and otherwise only main effects.
  
  # output: Returns the CV error estimate of the model described above.
  environment(repeated10FoldCV) = environment()
  mod = glm(cases ~ bs(years,n) + sexInd + age + counties + offset(log(pop)),family=poisson,data=dataFrame)
  repeatedCV = repeated10FoldCV(mod,dataFrame,5)
  return(repeatedCV)
}

plotSplineModels=function(dataFrame,n){
  # n = maximum number of degrees of freedom tried, must be greater than or equal to 3
  # input: n = maximum number of degrees of freedom in our cubic spline function for "years" we want
  # to try (n must be greater than or equal to 3), dataFrame = data.frame object used to build our 
  # regression model with splineModelDF.
  
  # output: Produces a plot showing the CV error estimates of models with different degrees of freedom
  # so we can compare them.
  
  knotVec = c(3:n)
  cvEstimates = c(numeric(length(n-2)))
  seEstimates = c(numeric(length(n-2)))
  for(i in 1:(n-2)){
    values = splineModelDF(dataFrame,knotVec[i])
    cvEstimates[i] = values$cvEstimate
    seEstimates[i] = values$seEstimate
  }
  minimumValue = min(cvEstimates-2*seEstimates)
  maximumValue = max(cvEstimates+2*seEstimates)
  
  plot(c(1:length(knotVec)),cvEstimates,type="n",xaxt="n",main="CV error estimates and degrees of freedom in cubic spline",
       xlab = "Degrees of freedom in cubic spline",ylim=c(minimumValue,maximumValue),ylab="CV error estimates")
  errbar(c(1:length(knotVec)),cvEstimates,cvEstimates+seEstimates,cvEstimates-seEstimates,
         xaxt="n",main="CV error estimates and degrees of freedom in cubic spline",ylab="CV error estimates",xlab="Degrees of freedom in cubic spline",add=T)
  axis(1,at=seq_along(cvEstimates),labels=as.character(knotVec),xlab="Degrees of freedom in cubic spline")
}

plotSplineModels(df4,10)
# Shows that df = 5 is the best choice out of the spline models. We'll compare the CV error estimate of the
# best spline model with df=5 with currentMod to see if it's within one standard error. It's a bit surprising a spline
# model with df = 5 is chosen.
splineModel = glm(cases ~ bs(years,df = 5)+sexInd + age + counties +offset(log(pop)),family=poisson,data=df4)
splineVal = repeated10FoldCV(splineModel,df4,5)
cat("Spline model's CV error estimate: ",splineVal$cvEstimate, ", spline models standard error estimate: ",
    splineVal$seEstimate)
# CV error estimate = 10.74701, standard estimate = 0.01622476
# This is within 1 standard error of currentModel, so we'll continue from this one until we find a better alternative.
# I'm a bit surprised the standard error is lower than for the models without splines.

# Try adding interaction terms
splineMod14 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age +offset(log(pop)),family=poisson,data=df4)
splineVal14 = repeated10FoldCV(splineMod14,df4,5)
# cvEstimate = 9.320802, seEstimate = 0.1681876
# Much lower than previously, this is the new "currently best model".

splineMod15 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age + sexInd:counties +offset(log(pop)),family=poisson,data=df4)
splineVal15 = repeated10FoldCV(splineMod15,df4,5)
# cvEstimate = 9.232731, seEstimate = 0.0173921
# Lower than previously, this is the new "currently best model"

splineMod16 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:bs(years,df=5)  +offset(log(pop)),family=poisson,data=df4)
splineVal16 = repeated10FoldCV(splineMod16,df4,5)
# cvEstimate = 9.104462, seEstimate = 0.01898734

splineMod17 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:bs(years,df=5) + age:counties + offset(log(pop)),family=poisson,data=df4)
splineVal17 = repeated10FoldCV(splineMod17,df4,5)
# splineVal17 = 9.34447, seEstimate = 0.06007234

splineMod18 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + 
                     sexInd:age + sexInd:counties + sexInd:bs(years,df=5) + 
                     age:counties +  age:bs(years,df=5)
                   + offset(log(pop)),family=poisson,data=df4)
splineVal18 = repeated10FoldCV(splineMod18,df4,5)
# cvEstimate = 6.455727, seEstimate = 0.02014712

splineMod19 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + 
                     sexInd:age + sexInd:counties + sexInd:bs(years,df=5) + 
                     age:counties +  age:bs(years,df=5)
                   + counties:bs(years,df=5)
                   + offset(log(pop)),family=poisson,data=df4)
splineVal19 = repeated10FoldCV(splineMod19,df4,5)
# cvEstimate = 5.203296, seEstimate = 0.04060531

splineMod20 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:bs(years,df=5) + 
                     age:counties + sexInd:age:counties +offset(log(pop)),family=poisson,data=df4)
splineVal20 = repeated10FoldCV(splineMod20,df4,5)
# cvEstimate = 9.487041, seEstimate = 0.08544412

# This is a bit arbitrary, but you realize by yourself that adding further three- or four-factor interaction terms will be a waste of time. It's extremely
# unlikely this will produce a better fit. This is your gut feeling at least, might try it for fun but you probably won't focus on it.

# Used a hard coded plot for these models to make it 
splineCVValues = c(splineVal14$cvEstimate,splineVal15$cvEstimate,splineVal16$cvEstimate,splineVal17$cvEstimate,
                   splineVal18$cvEstimate,splineVal19$cvEstimate,splineVal20$cvEstimate)
splineSEValues = c(splineVal14$seEstimate,splineVal15$seEstimate,splineVal16$seEstimate,splineVal17$seEstimate,
                   splineVal18$seEstimate,splineVal19$seEstimate,splineVal20$seEstimate)

interactionTerms = c("sexInd:age",
                      "+sexInd:counties",
                      "+sexInd:bs(years,df=5)",
                      "+age:counties",
                      "+age:bs(years,df=5)",
                      "+counties:bs(years,df=5)",
                      "sexInd:age:counties"
                      )
# Everything after "sexInd:age" tells us what we added to the previous one. I.e. 
# "+sexInd:counties" means the model with sexInd:age + sexInd:counties and "+sexInd:bs(years,df=6)"
# means the model with sexInd:age + sexInd:counties + sexInd:bs(years,df=6)
# The models plotted above have increasing complexity, i.e. more terms are added

# New way
maxVal = max(splineCVValues+2*splineSEValues)
minVal = min(splineCVValues-2*splineSEValues)
plot(c(1:length(interactionTerms)),splineCVValues,type="n",xaxt="n",main="CV error estimates of models with interaction terms",
     xlab = "",ylim=c(minVal,maxVal),ylab="CV error estimates")
errbar(c(1:length(interactionTerms)),splineCVValues,splineCVValues+splineSEValues,splineCVValues-splineSEValues,
       xaxt="n",main="CV error estimates of models with interaction terms",ylab="CV error estimates",xlab="",add=T)
box()
axis(1,at=seq_along(splineCVValues),labels=as.character(interactionTerms),xlab="")
# As we can see, splineMod19 blows everything else out of the water and is clearly the best model
# out of all the ones we have considered so far.

plotRegressionModels = function(dataFrame,formulaVec){
  # input: dataFrame is the data.frame object used to build our regression models, formulaVec is a vector
  # of strings which are the formulas used in our regression models.
  
  # output: Produces a plot of the CV error estimates for our different models so we can compare them.
  
  environment(repeated10FoldCV) = environment()
  
  cvEstimates = c(numeric(length(formulaVec)))
  seEstimates = c(numeric(length(formulaVec)))
  n = length(cvEstimates)
  for(i in 1:n){
    form = as.formula(formulaVec[i])
    mod = glm(form,family=poisson,data=dataFrame)
    value = repeated10FoldCV(mod,dataFrame,5)
    cvEstimates[i] = value$cvEstimate
    seEstimates[i] = value$seEstimate
  }
  maxValue = max(cvEstimates+2*seEstimates)
  minValue = min(cvEstimates-2*seEstimates)
  
  plot(c(1:length(formulaVec)),cvEstimates,type="n",xaxt="n",main="Removed variables and CV error estimates",
       xlab = "",ylim=c(minValue,maxValue),ylab="CV error estimates")
  errbar(c(1:length(formulaVec)),cvEstimates,cvEstimates+seEstimates,cvEstimates-seEstimates,
         xaxt="n",main="Removed variables and CV error estimates",ylab="CV error estimates",xlab="",add=T,
         errbar.col = c("yellow","red","green","black","purple"))
  legend("topleft",legend=formulaVec,fill=c("yellow","red","green","black","purple"),ncol=1,cex=0.7,xpd=TRUE)
  box()
}

str1 = "cases ~ bs(years,df=5) + age + sexInd + counties + offset(log(pop))"
str2 = "cases ~ sexInd + age + counties + offset(log(pop))"
str3 = "cases ~ bs(years,df=5) + age + counties + offset(log(pop))"
str4 = "cases ~ bs(years,df=5) + sexInd + age+ offset(log(pop))"
str5 = "cases ~ bs(years,df=5) + sexInd + counties + offset(log(pop))"
plotRegressionModels(df4,c(str1,str2,str3,str4,str5))
# Removing variables has a disastrous effect


# We choose splineMod19 as the best model among the considered ones and take a
# look at the diagnostic plots below.
diagnosticPlots(splineMod19,df4)

# Extension of what you did before
# Instead of using the spline function in the interaction terms, you try only using "years". You'll use a hard coded plot for this.
splineMod14 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age +offset(log(pop)),family=poisson,data=df4)
splineMod15 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age + sexInd:counties +offset(log(pop)),
                   family=poisson,data=df4)
splineMod21 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:years
                   +offset(log(pop)),family=poisson,data=df4)
splineMod22 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:years
                   + age:counties + offset(log(pop)),family=poisson,data=df4)
splineMod23 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + 
                     sexInd:age + sexInd:counties + sexInd:years + 
                     age:counties +  age:years
                   + offset(log(pop)),family=poisson,data=df4)
splineMod24 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + 
                     sexInd:age + sexInd:counties + sexInd:years + 
                     age:counties +  age:years
                   + counties:years
                   + offset(log(pop)),family=poisson,data=df4)
splineMod25 = glm(cases ~ bs(years,df=5) + sexInd + age + counties + sexInd:age + sexInd:counties + sexInd:years 
                   + age:counties + sexInd:age:counties +offset(log(pop)),family=poisson,data=df4)

interactionTerms2 = c("sexInd:age",
                      "+sexInd:counties",
                      "+sexInd:years",
                      "+age:counties",
                      "+age:years",
                      "+counties:years",
                      "sexInd:age:counties")
splineVal21 = repeated10FoldCV(splineMod21,df4,5) # Goes pretty fast
splineVal22 = repeated10FoldCV(splineMod22,df4,5) # Started 23.55, finished 00.08
splineVal23 = repeated10FoldCV(splineMod23,df4,5) # Started 00.08, finished by 00.20 (possible earlier)
splineVal24 = repeated10FoldCV(splineMod24,df4,5) # Started 00.20, finished by 00.35
splineVal25 = repeated10FoldCV(splineMod25,df4,5) # Started 00.35, finished 01.32

# Make the hard coded plot
hardCodedCVVec = c(splineVal14$cvEstimate,splineVal15$cvEstimate,splineVal21$cvEstimate,splineVal22$cvEstimate,splineVal23$cvEstimate,splineVal24$cvEstimate,
                   splineVal25$cvEstimate)
hardCodedSEVec = c(splineVal14$seEstimate,splineVal15$seEstimate,splineVal21$seEstimate,splineVal22$seEstimate,splineVal23$seEstimate,splineVal24$seEstimate,
                   splineVal25$seEstimate)
hardCodedMinVal = min(hardCodedCVVec-2*hardCodedSEVec)
hardCodedMaxVal = max(hardCodedCVVec+2*hardCodedSEVec)
plot(c(1:length(interactionTerms2)),hardCodedCVVec,type="n",xaxt="n",main="Interaction terms and CV error estimates",
     xlab = "",ylim=c(hardCodedMinVal,hardCodedMaxVal),ylab="CV error estimates")
errbar(c(1:length(interactionTerms2)),hardCodedCVVec,hardCodedCVVec+hardCodedSEVec,hardCodedCVVec-hardCodedSEVec,
       xaxt="n",main="CV error estimates of models with interaction terms",ylab="CV error estimates",xlab="",add=T)
box()
axis(1,at=seq_along(splineCVValues),labels=as.character(interactionTerms2),xlab="")
# None of the models beat splineMod19