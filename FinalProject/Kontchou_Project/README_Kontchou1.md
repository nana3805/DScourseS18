
# Who is the Greatest Fighter of All Time?

This project uses RStudio to analyze different factors and trends among athletes competing in the Ultimate Fighting Championship. The analysis can be considered as an outline for an argument for the greatest fighter of all time.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

There are several RStudio packages that need to be installed before running this code. Here they are:

```
install.packages("data.table")
install.packages("lubridate")
install.packages("ggthemes")
install.packages("ggplot2")
install.packages("stringr")
install.packages("plotly")
install.packages("dplyr")
```

### Create a list of fighters based on win percentage

Read the data sets into the project and convert them to tables.

```
Fighters <- read.csv('Fighters_Updated.csv')[,-1]
Fights <- read.csv('Fights_Updated.csv')[,-1]
FightersTable <- data.table::as.data.table(Fighters)
Fights <- data.table::as.data.table(Fights)
```
### Clean Data

Remove the fights that ended in draws or no contents. Remove the "win" from the last name of the entries in Fighter1 column in Fights data frame

```
Fights <- Fights[grepl("draw", Fights$Fighter1, fixed = TRUE) == FALSE & grepl("NC", Fights$Fighter1, fixed = TRUE) == FALSE,]
Fights <- transform(Fights, F1Result = rep("win", length(Fights$Fighter1)))
```

Set key on fighters data table and remove duplicate entries

```
as.character(FightersTable$Fighter_id)
setkey(FightersTable, Fighter_id)
ufcfighters <- unique(FightersTable)
```

Remove non-numeric values from Fighter1_url to match Fighter_id

```
Fights <- transform(Fights, Fighter1ID = -1*(as.numeric(str_extract(Fighter1_url, "\\-*\\d+\\.*\\d*"))))
Fights <- transform(Fights, Fighter2ID = -1*(as.numeric(str_extract(Fighter2_url, "\\-*\\d+\\.*\\d*"))))
```

Merge the two tables by win percentage

```
setkey(Fights, Fighter1ID)
merged <- Fights[FightersTable, .(Name, Class, Age, wins=sum(F1Result=="win"), fights1=.N), by=.EACHI]
setkey(Fights, Fighter2ID)
merged2 <- Fights[merged, .(Name, Class, Age, wins, fights1, fights2=.N), by=.EACHI][
  is.na(wins), wins := 0][
    ,`:=`(Bouts = fights1 + fights2,
          win_perc =wins/(fights1 + fights2))]
```

### Emperical Bayes Equilibrium Equation

```
merged2_sample <- merged2[win_perc > 0 & win_perc < 1]
m1 = 0.5
m2 = sum(merged2_sample$win_perc ^ 2)/length(merged2_sample$win_perc)
alpha0 = (m1*(m1-m2))/(m2 - m1^2)
beta0 = ((1-m1)*(m1-m2))/(m2-m1^2)
merged3 <- merged2[,win_perc_adj := (wins + alpha0) / (Bouts + alpha0 + beta0)]
```

## Running the tests to get a list of the top 3 fighters, all fighers by adjusted win percentage, and rankings in weight class.

```
Top3 <- merged2[order(-win_perc_adj), .(Name, Class, Bouts, wins, win_perc_adj)][1:3]
Pound_for_Pound <- merged2[order(-win_perc_adj), .(Name, Class, Bouts, wins, win_perc_adj)][1:2000]
UFC_Rankings <- merged2[order(-win_perc_adj), .(Name, Class, Bouts, Age, wins, win_perc_adj)][,head(.SD,5), by=Class]
```
## Running the tests for wins by Jon Jones, GSP, Demetrious Johnson
```
JonJones_Wins <- Fights1[(grep(27944, Fights1$Fighter1ID))] 
GSP_Wins <- Fights1[(grep(3500, Fights1$Fighter1ID))] 
DJohnson_Wins <- Fights1[(grep(45452, Fights1$Fighter1ID))] 
```

### Methods of victory 

This gives a breakdown of the different methods of victory for each of the top 3 fighters, their weight classes, and the entire UFC. This gives a measurement of margin for error (chances the fight does not go to judges' decision) within each weight class, and the percentage of finishes in each of the top 3 fighters careers.

```
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

```

### Adjusted win percentage of the opponents for the top 3

These tests measure the quality of opposition that each of the top 3 fighters faced to assess the level of difficulty in their careers

```
JJLosers <- merge(JonJones_Wins,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
GSPLosers <- merge(GSP_Wins,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
DJLosers <- merge(DJohnson_Wins,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
JJWinners <- merge(JonJones_Losses,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
GSPWinners <- merge(GSP_Losses,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
DJWinners<- merge(DJohnson_Losses,merged3[,c('Name','Class','win_perc_adj','Fighter2ID')],by.x='Fighter2ID',by.y='Fighter2ID')
summary(JJLosers$win_perc_adj)
summary(GSPLosers$win_perc_adj)
summary(DJLosers$win_perc_adj)

```

## Win-rate in the UFC with age

```
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

```
## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **KevinKontchou** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Jian Qiao and Karim Lahrichi for thier useful code

