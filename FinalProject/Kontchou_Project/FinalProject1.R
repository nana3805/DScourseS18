#Read packages
library(data.table)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(stringr)
library(plotly)
library(dplyr)

#Import data sets
Fighters <- read.csv('Fighters_Updated.csv')[,-1]
Fights <- read.csv('Fights_Updated.csv')[,-1]

#Convert data into table
FightersTable <- data.table::as.data.table(Fighters)
Fights <- data.table::as.data.table(Fights)

#Clean data
Fights <- Fights[grepl("draw", Fights$Fighter1, fixed = TRUE) == FALSE & grepl("NC", Fights$Fighter1, fixed = TRUE) == FALSE,]
Fights <- transform(Fights, F1Result = rep("win", length(Fights$Fighter1)))

#Set key on fighters data table and remove duplicate entries
as.character(FightersTable$Fighter_id)
setkey(FightersTable, Fighter_id)
ufcfighters <- unique(FightersTable)

#Remove non-numeric values from Fighter1_url to match Fighter_id
Fights <- transform(Fights, Fighter1ID = -1*(as.numeric(str_extract(Fighter1_url, "\\-*\\d+\\.*\\d*"))))
Fights <- transform(Fights, Fighter2ID = -1*(as.numeric(str_extract(Fighter2_url, "\\-*\\d+\\.*\\d*"))))

#Set key on fights data
setkey(Fights, Fighter1ID)

#Merge fight data onto fighters data.
#For each fighter, the number of wins is equal to the number of times he is Fighter1 and the result for fighter1 is a win.
#Similarly, I stored the number of times he or she fought as fighter1.
merged <- Fights[FightersTable, .(Name, Class, Age, wins=sum(F1Result=="win"), fights1=.N), by=.EACHI]

#Now change the key of fights data so we can count the # of times each fighter was fighter2.
setkey(Fights, Fighter2ID)

#Compute the total number of fights for each fighter, and their win perentage.
merged2 <- Fights[merged, .(Name, Class, Age, wins, fights1, fights2=.N), by=.EACHI][
  is.na(wins), wins := 0][
    ,`:=`(Bouts = fights1 + fights2,
          win_perc =wins/(fights1 + fights2))]

#Emperical Bayes Equilibrium Equation
merged2_sample <- merged2[win_perc > 0 & win_perc < 1]
m1 = 0.5
m2 = sum(merged2_sample$win_perc ^ 2)/length(merged2_sample$win_perc)
alpha0 = (m1*(m1-m2))/(m2 - m1^2)
beta0 = ((1-m1)*(m1-m2))/(m2-m1^2)
merged3 <- merged2[,win_perc_adj := (wins + alpha0) / (Bouts + alpha0 + beta0)]

#Create comprehensive list based on adjusted win percentage
Top3 <- merged2[order(-win_perc_adj), .(Name, Class, Bouts, wins, win_perc_adj)][1:3]
Pound_for_Pound <- merged2[order(-win_perc_adj), .(Name, Class, Bouts, wins, win_perc_adj)][1:2000]
UFC_Rankings <- merged2[order(-win_perc_adj), .(Name, Class, Bouts, Age, wins, win_perc_adj)][,head(.SD,5), by=Class]

#Histogram of win percentages
hist_win_perc <- hist(merged3$win_perc_adj,
                 main="Histogram for Win Percentages", 
                 xlab="Win Percentages", 
                 border="blue",
                 ylim = c(0, 400),
                 col="green",
                 las=1, 
                 breaks=5)

#More data cleaning (Extract numbers from Fighter IDs)
Fights$Fighter1_id <- gsub('.*-','',Fights$Fighter1_url)
Fights$Fighter2_id <- gsub('.*-','',Fights$Fighter2_url)
Fighters$Fighter_id <- as.character(Fighters$Fighter_id)
Fights1 <- merge(Fights,Fighters[,c('Weight','Class','Fighter_id')],by.x='Fighter1_id',by.y='Fighter_id')

#Fights by Jon Jones, GSP, Demetrious Johnson
JonJonesFights <- Fights1[with(Fights1, grepl(27944, Fights1$Fighter1ID)|grepl(27944, Fights1$Fighter2ID)),]
GSP_Fights <- Fights1[with(Fights1, grepl(3500, Fights1$Fighter1ID)|grepl(3500, Fights1$Fighter2ID)),]
DemetriousJohsonFights <- Fights1[with(Fights1, grepl(45452, Fights1$Fighter1ID)|grepl(45452, Fights1$Fighter2ID)),]

#Wins by Jon Jones, GSP, Demetrious Johnson
JonJones_Wins <- Fights1[(grep(27944, Fights1$Fighter1ID))] 
GSP_Wins <- Fights1[(grep(3500, Fights1$Fighter1ID))] 
DJohnson_Wins <- Fights1[(grep(45452, Fights1$Fighter1ID))] 

#Losses by Jon Jones, GSP, Demetrious Johnson
JonJones_Losses <- Fights1[(grep(27944, Fights1$Fighter2ID))] 
GSP_Losses <- Fights1[(grep(3500, Fights1$Fighter2ID))] 
DJohnson_Losses <- Fights1[(grep(45452, Fights1$Fighter2ID))] 

#Compare the quality of opposition the top 3 faced
JJLosers <- merge(JonJones_Wins,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
GSPLosers <- merge(GSP_Wins,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
DJLosers <- merge(DJohnson_Wins,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
JJWinners <- merge(JonJones_Losses,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
GSPWinners <- merge(GSP_Losses,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
DJWinners<- merge(DJohnson_Losses,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
summary(JJLosers$win_perc_adj)
summary(GSPLosers$win_perc_adj)
summary(DJLosers$win_perc_adj)

#Fights by class
Heavyweights <- Fights1[(grep("Heavyweight", Fights1$Class))] 
Heavyweight_Fights <- Heavyweights[Heavyweights$Weight>206]
Light_Heavyweight_Fights <- Fights1[(grep("Light Heavyweight", Fights1$Class))]
Middleweight_Fights <- Fights1[(grep("Middleweight", Fights1$Class))]
Welterweight_Fights <- Fights1[(grep("Welterweight", Fights1$Class))]
Lightweight_Fights <- Fights1[(grep("Lightweight", Fights1$Class))]
Featherweight_Fights <- Fights1[(grep("Featherweight", Fights1$Class))]
Flyweight_Fights <- Fights1[(grep("Flyweight", Fights1$Class))]


#Pie chart for methods of victory for all fighters
Method <- Fights %>% group_by(Method) %>% summarise(Count=n())
MethodMethod <- Method$Method
pie_chart_methods <- plot_ly(Method, labels = Method$Method, values = Method$Count, type = 'pie', 
                                 textposition = 'outside',
                                 textinfo = 'label+percent',
                                 insidetextfont = list(color = '#FFFFFF'),
                                 hoverinfo = 'text',
                                 text = ~paste(MethodMethod),
                                 marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                                 showlegend = FALSE) %>%
layout(title = 'Methods of Victory',
xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Pie chart for methods of victory for Jon Jones
Method_JonJones <- JonJones_Wins %>% group_by(Method) %>% summarise(Count=n())
MethodJonJonesMethod <- Method_JonJones$Method
pie_chart_methods_jonjones <- plot_ly(Method_JonJones, labels = Method_JonJones$Method, values = Method_JonJones$Count, type = 'pie', 
                                  textposition = 'outside',
                                  textinfo = 'label+percent',
                                  insidetextfont = list(color = '#FFFFFF'),
                                  hoverinfo = 'text',
                                  text = ~paste(MethodJonJonesMethod),
                                  marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                                  showlegend = FALSE) %>%
layout(title = 'Methods of Victory for Jon Jones',
xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Pie chart for methods of victory for Georges St. Pierre
Method_GSP<- GSP_Wins %>% group_by(Method) %>% summarise(Count=n())
MethodGSPMethod <- Method_GSP$Method
pie_chart_methods_gsp <- plot_ly(Method_GSP, labels = Method_GSP$Method, values = Method_GSP$Count, type = 'pie', 
                                  textposition = 'outside',
                                  textinfo = 'label+percent',
                                  insidetextfont = list(color = '#FFFFFF'),
                                  hoverinfo = 'text',
                                  text = ~paste(MethodGSPMethod),
                                  marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                                  showlegend = FALSE) %>%
layout(title = 'Methods of Victory for Georges St. Pierre',
xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Pie chart for methods of victory for Demetrious Johnson
Method_DJohnson<- DJohnson_Wins %>% group_by(Method) %>% summarise(Count=n())
MethodDJohnsonMethod <- Method_DJohnson$Method
pie_chart_methods_djohnson <- plot_ly(Method_DJohnson, labels = Method_DJohnson$Method, values = Method_DJohnson$Count, type = 'pie', 
                                  textposition = 'outside',
                                  textinfo = 'label+percent',
                                  insidetextfont = list(color = '#FFFFFF'),
                                  hoverinfo = 'text',
                                  text = ~paste(MethodDJohnsonMethod),
                                  marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                                  showlegend = FALSE) %>%
layout(title = 'Methods of Victory for Demetrious Johnson',
xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Pie chart for methods of victory for light heavyweights
Light_Heavyweight_Fights <- Fights1[(grep("Light Heavyweight", Fights1$Class))]
Method_Light_Heavy <- Light_Heavyweight_Fights %>% group_by(Method) %>% summarise(Count=n())
MethodLightHeavyMethod <- Method_Light_Heavy$Method
pie_chart_methods_light_heavy <- plot_ly(Method_Light_Heavy, labels = Method_Light_Heavy$Method, values = Method_Light_Heavy$Count, type = 'pie', 
                                    textposition = 'outside',
                                    textinfo = 'label+percent',
                                    insidetextfont = list(color = '#FFFFFF'),
                                    hoverinfo = 'text',
                                    text = ~paste(MethodLightHeavyMethod),
                                    marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                                    showlegend = FALSE) %>%
layout(title = 'Methods of Victory for Light Heavyweights',
xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Pie chart for methods of victory for welterweights
Welterweight_Fights <- Fights1[(grep("Welterweight", Fights1$Class))]
Method_Welter <- Welterweight_Fights %>% group_by(Method) %>% summarise(Count=n())
MethodWelterMethod <- Method_Welter$Method
pie_chart_methods_welter <- plot_ly(Method_Welter, labels = Method_Welter$Method, values = Method_Welter$Count, type = 'pie', 
                                    textposition = 'outside',
                                    textinfo = 'label+percent',
                                    insidetextfont = list(color = '#FFFFFF'),
                                    hoverinfo = 'text',
                                    text = ~paste(MethodWelterMethod),
                                    marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                                    showlegend = FALSE) %>%
layout(title = 'Methods of Victory for Welterweights',
xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Pie chart for methods of victory for flyweights
Flyweight_Fights <- Fights1[(grep("Flyweight", Fights1$Class))]
Method_Fly <- Flyweight_Fights %>% group_by(Method) %>% summarise(Count=n())
MethodFlyMethod <- Method_Fly$Method
pie_chart_methods_fly <- plot_ly(Method_Fly, labels = Method_Fly$Method, values = Method_Fly$Count, type = 'pie', 
                                    textposition = 'outside',
                                    textinfo = 'label+percent',
                                    insidetextfont = list(color = '#FFFFFF'),
                                    hoverinfo = 'text',
                                    text = ~paste(MethodFlyMethod),
                                    marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                                    showlegend = FALSE) %>%
layout(title = 'Methods of Victory for Flyweights',
xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Merging data sets to get fighting age of fighters
Fights <- merge(Fights,Fighters[,c('Birth_Date','Fighter_id')],by.x='Fighter1_id',by.y='Fighter_id')
Fights <- merge(Fights,Fighters[,c('Birth_Date','Fighter_id')],by.x='Fighter2_id',by.y='Fighter_id')

Fights$Date <- mdy(Fights$Date)
Fights$Birth_Date.x <- mdy(format(as.Date(Fights$Birth_Date.x, "%m/%d/%y"), "%m/%d/19%y"))
Fights$Birth_Date.y <- mdy(format(as.Date(Fights$Birth_Date.y, "%m/%d/%y"), "%m/%d/19%y"))

#Calculating Fighting Age.
Fights$Fighter1_Age <- year(Fights$Date)-year(Fights$Birth_Date.x)
Fights$Fighter2_Age <- year(Fights$Date)-year(Fights$Birth_Date.y)

Fighting_Age <- as.data.frame(unlist(Fights[,c('Fighter1_Age','Fighter2_Age')]))
colnames(Fighting_Age) <- 'Age'
Fighting_Age <- Fighting_Age %>% group_by(Age) %>% summarise(Count=n())

#Win-Rate against Age
#Win-Rate drop with Age
Fights$Win <- 1
Fights$Loss <- 1
Fights$Draw <- 0
Fights$Win[Fights$Method %in% c('Draw','No Contest','No Decision')] <- 0
Fights$Loss[Fights$Method %in% c('Draw','No Contest','No Decision')] <- 0
Fights$Draw[Fights$Method %in% c('Draw')] <- 1

Win_by_Age <- Fights[,c('Fighter1_Age','Win')]
Loss_by_Age <- Fights[,c('Fighter2_Age','Loss')]
temp1 <- Fights[,c('Fighter1_Age','Draw')]
temp2 <- Fights[,c('Fighter2_Age','Draw')]
colnames(temp1) <- c('Age','Draw')
colnames(temp2) <- c('Age','Draw')
Draw_by_Age <- rbind(temp1,temp2)
rm(temp1)
rm(temp2)

#Win-Rate By Age
Win_by_Age <- Win_by_Age %>% group_by(Fighter1_Age) %>% summarise(Numbers=sum(Win))
Loss_by_Age <- Loss_by_Age %>% group_by(Fighter2_Age) %>% summarise(Numbers=sum(Loss))
Draw_by_Age <- Draw_by_Age%>% group_by(Age) %>% summarise(Numbers=sum(Draw))

Win_Rate_by_Age <- merge(merge(Win_by_Age,Loss_by_Age,by.x='Fighter1_Age',by.y='Fighter2_Age'),Draw_by_Age,by.x='Fighter1_Age',by.y='Age')
colnames(Win_Rate_by_Age) <- c('Age','Wins','Loss','Draws')
Win_Rate_by_Age$Win_Rate <- Win_Rate_by_Age$Wins/(Win_Rate_by_Age$Wins+Win_Rate_by_Age$Loss+Win_Rate_by_Age$Draws)

#Distribution of fighters' number of wins by age
Win_Rate_by_Age$Bouts <- Win_Rate_by_Age$Wins + Win_Rate_by_Age$Loss + Win_Rate_by_Age$Draws
plot(Win_Rate_by_Age$Age, Win_Rate_by_Age$Bouts, main = "Distribution of Bouts by Age", xlab = "Fighter Age", ylab = "Number of Bouts")

#Line Graph of fighters' win rate with respect to age
g_Win_Rate_by_Age <- ggplot(data=Win_Rate_by_Age,aes(x=Age,y=Win_Rate))+geom_line(color='white')+geom_smooth(se=FALSE)
g_Win_Rate_by_Age <- g_Win_Rate_by_Age+theme_hc()
g_Win_Rate_by_Age <- g_Win_Rate_by_Age+labs(x='Age',y='Win Rate')
g_Win_Rate_by_Age <- g_Win_Rate_by_Age+theme(axis.text=element_text(size=14,colour='white'),axis.title=element_text(size=20,colour='white'),plot.title=element_text(size=25,colour='white'))
g_Win_Rate_by_Age <- g_Win_Rate_by_Age+theme(panel.background = element_blank(),panel.grid.major = element_line(colour='white'),plot.background = element_rect(fill='#272b30'))
g_Win_Rate_by_Age <- g_Win_Rate_by_Age+scale_color_manual(values=c('white','blue'))
g_Win_Rate_by_Age <- g_Win_Rate_by_Age+ggtitle("Success with Age")

