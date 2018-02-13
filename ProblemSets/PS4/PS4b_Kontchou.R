sparkR
df1<- as.data.frame(iris)
df<- createDataFrame(iris)
class(df1)
class(df)
head(select(df, df$Sepal_Length, df$Species))
head(filter(df, df$Sepal_Length>5.5))
head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))