library(WDI)
library(ggplot2)
library(googleVis)
library(plyr)
library(readxl)

excel <- readxl::read_excel("/Users/kevinkontchou/Documents/CameroonGDPGrowth.xlsx")
GDP <- WDI(indicator='NY.GDP.MKTP.CD', country="CM",start=1960, end=2016)
lifeExpectancy <- WDI(indicator='SP.DYN.LE00.IN', country="CM",start=1960, end=2016)
fertility = WDI(indicator='SP.DYN.TFRT.IN', country="CM",start=1960, end=2016)
population = WDI(indicator='SP.POP.TOTL', country="CM",start=1960, end=2016)

names(lifeExpectancy)[3]="Life Expectancy (Years)"
names(GDP)[3]="GDP (US$)"
names(population)[3]="Population"
names(fertility)[3]="Fertility (Births per woman)"

j1 <- join(lifeExpectancy, GDP)
j2 <- join(j1, population)
j3 <- join(j2, fertility)

LifeExpectanyLine <- ggplot(j3, aes(x = year, y = `Life Expectancy (Years)`)) + geom_line()
CameroonGDPGrowth <- ggplot(excel, aes(x = Date, y = `Real GDP Growth, %`)) + geom_line()
fertilityline <- ggplot(j3, aes(x = year, y = `Fertility (Births per woman)`)) + geom_line()

print(LifeExpectanyLine + ggtitle("            Life Expectancy in Cameroon Line Graph"))
print(CameroonGDPGrowth + ggtitle("            Growth of Cameroon GDP Line Graph"))
print(fertilityline + ggtitle("            Fertility Rate in Cameroon Line Graph"))


