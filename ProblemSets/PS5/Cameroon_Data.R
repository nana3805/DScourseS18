library(WDI)
library(ggplot2)
library(googleVis)
library(plyr)

GDP <- WDI(indicator='NY.GDP.MKTP.CD', country="CM",start=1960, end=2016)
lifeExpectancy <- WDI(indicator='SP.DYN.LE00.IN', country="CM",start=1960, end=2016)
fertility = WDI(indicator='SP.DYN.TFRT.IN', country="CM",start=1960, end=2016)
elecConsumption= WDI(indicator='EG.USE.ELEC.KH.PC', country="CM",start=1960, end=2016)


names(lifeExpectancy)[3]="Life Expectancy (Years)"
names(GDP)[3]="GDP (US$)"
names(elecConsumption)[3]="Electric power consumption (KWH per capita)"
names(fertility)[3]="Fertility (Births per woman)"

j1 <- join(lifeExpectancy, GDP)
j2 <- join(j1, elecConsumption)
j3 <- join(j2, fertility)


