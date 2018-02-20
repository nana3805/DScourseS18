#loading rvest package
library(rvest)

#specify url
url <- "https://en.wikipedia.org/wiki/List_of_African_countries_by_GDP_(nominal)"

#read html code from webpage
webpage <- read_html(url)

#compile data into table
test <- html_nodes(webpage, "table")
test2 <- html_table (test[[1]], fill = TRUE)

head(test2)