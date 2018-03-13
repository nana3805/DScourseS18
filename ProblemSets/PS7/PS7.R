#install.packages("readr")
#install.packages("knitr")
#install.packages("MixedDataImpute")
#install.packages("mice")
#install.packages("stargazer")

#load packages
library(MixedDataImpute)
library(mice)
library(stargazer)
library(readr)
library(knitr)

#Import Data
mydata <- "https://raw.githubusercontent.com/tyleransom/DScourseS18/master/ModelingOptimization/wages.csv"
WagesData <- read.csv(mydata)

#drop observations where tenure or hgc are missing
WagesData <- subset(WagesData, !is.na(tenure))
WagesData <- subset(WagesData,!is.na(hgc))
stargazer(WagesData)

#linear regression model
f1 <- as.formula("logwage ~ hgc + college + tenure + tenure^2 + age + married")
mod <- lm(f1, WagesData)
summary(mod)

#Listwise deletion on log wage variable
WagesData1 <- WagesData[!is.na(WagesData$logwage),]
mod1 <- lm(f1, WagesData1)
summary(mod1)

#Perform mean imputation to fill in missing log wages
WagesData2 <- transform(WagesData, logwage = ifelse(is.na(logwage), mean(logwage, na.rm=TRUE), logwage))
mod2 <- lm(f1, WagesData2)
summary(mod)

#MICE package imputation
library(mice)
WagesData.imp <- mice(WagesData, seed = 12345)
summary(WagesData)
fit <- with(WagesData.imp, lm(f1, WagesData))
round(summary(pool(fit)),2)

#regression results
stargazer(mod, mod1, mod2, title = "Regression Results")


