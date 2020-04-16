
#####################################
#   Data Acquistion
#####################################

# Reading dataset from directory

getwd()
#setwd("C:/Personal/")

# Reading dataset using read.csv command

movie_df <- read.csv("movie_metadata.csv")
movie_df <- read.csv(file.choose())

str(movie_df)
names(movie_df)
summary(movie_df)
class(movie_df)
nrow(movie_df)
ncol(movie_df)

#####################################
#   Download Raw data from Website
#####################################

LinkAddress = "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
#dest should be file name and not a directory
download.file(LinkAddress, dest="C:/Personal/Module 3/webdata/test2.csv")
list.files("C://Personal/Module 3/webdata")

#####################################
#   Webscrapping - Reading from website
#####################################

#let's try the package rvest and IMDB website

install.packages('rvest')
library(rvest)
#Knowledge of selector gadget to understand HTML and CSS Tags

#Specifying the url for desired website to be scraped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)
#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#Using CSS selectors to scrape the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Let's have a look at the description data
head(description_data)

try <- as.data.frame(cbind(rank_data,title_data,description_data))
write.csv(try, "C:/Personal/Module 3/webdata/try.csv")

#####################################
#   Reading Excel File
#####################################
install.packages("xlsx")
library(xlsx)

InsuranceData <- read.xlsx("M3_Insurance_Data.xlsx", sheetIndex=1, header=TRUE)
#view(InsuranceData)

colIndex <- c(2:3)
rowIndex <- c(2:5)
InsuranceData_subset <- read.xlsx("M3_Insurance_Data.xlsx", sheetIndex = 1, colIndex = colIndex, rowIndex = rowIndex, header=TRUE)
InsuranceData_subset

#####################################
#   Reading from MSSQL
#####################################

library("RMySQL")
#Connect to a database

hg19 <- dbConnect(MySQL(), user="genome", db="hg19", host="genome-mysql.cse.ucsc.edu")

allTables <- dbListTables(hg19)
dbGetQuery(hg19, 'select count(*) from affyU133Plus2')

#####################################
#   Reading XML File
#####################################

library(XML)
library("methods")

# Give the input file name to the function.
result <- xmlParse(file="trial.xml")

a <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/ID")),"ID")    
b <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/NAME")),"Name") 
c <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/SALARY")),"Salary") 
d <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/STARTDATE")),"StartDate")
e <- setNames(xmlToDataFrame(node=getNodeSet(result,"//RECORDS/EMPLOYEE/DEPT")),"Department") 

employee_df <- cbind(a,b,c,d,e)

#####################################
#   Reading JSON File
#####################################
library(RJSONIO)
raw<- fromJSON("https://data.ny.gov/api/views/9a8c-vfzj/rows.json?accessType=DOWNLOAD")
class(raw)

employee_df[c(1:5),c(2:5)]

data <- raw[['data']]
data[[1]]
data[[1]][[14]]
data[[2]][[14]]

data[[1]][[1]]

#####################################
#   Reading from API
#####################################
install.packages("twitteR")
library(twitteR)

consumer_key <- 
consumer_secret <-
access_token <- 
access_secret <- 

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Use searchTwitter to search Twitter based on the supplied search string and return a list. The “lang” parameter is used below to restrict tweets to the “English” language. 
tweets <- searchTwitter(searchString = "CORONA",20, lang="en")
tweets

#Getting back to movie dataset

movie_df <- read.csv("movie_metadata.csv")
movie_df <- file.choose()

class(movie_df)
summary(movie_df)

#Fetch 4th column from 1st Row ; df[RowIndex, ColIndex]
movie_df[1,4]

#Top 6 rows along with header name
head(movie_df)
tail(movie_df)

#Structure of dataset
str(movie_df)

movie_df[1, "duration"]
movie_df[1, c("movie_title", "duration")]
movie_df[1, 1:5] #first 5 columns
movie_df[1:5, 1:5] #first 5 rows and 5 cols

#All movies which duration > 200 mins
movie_df[movie_df$duration > 200, "movie_title"]

#Top 10 movies based on gross
gross_trial <- head(movie_df[order(movie_df$gross, decreasing = TRUE), c("movie_title", "gross")], 10)

#Top 10 movies with highest profits

movie_df$profit <- movie_df$gross - movie_df$budget
head(movie_df[order(movie_df$profit, decreasing = TRUE), c("movie_title", "gross", "profit")], 10)

#Know top rated movies in the list and also average IMDB Score
head(movie_df[order(movie_df$imdb_score, decreasing = TRUE), c("movie_title", "imdb_score")], 10)
mean(movie_df$imdb_score)

#aggregate function introduction

values <- data.frame(value = c("a", "a", "a", "a", "a", 
                               "b", "b", "b", 
                               "c", "c", "c", "c"))
nr.of.appearances <- aggregate(x = values, 
                               by = list(unique.values = values$value), 
                               FUN = length)

# aggregate data frame mtcars by cyl and vs, returning means
# for numeric variables
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,vs),
                    FUN=mean, na.rm=TRUE)
print(aggdata)

#Plot to show number of movies release each year #FUN ==> sum/mean/length
groupby_year <- aggregate(movie_df$movie_title, by = list(movie_df$title_year), FUN=length)
View(groupby_year)
plot(groupby_year)
groupby_year <- aggregate(movie_df$movie_title, by = list(movie_df$title_year, movie_df$language), FUN=length)

#####################################
#  Techniques used in Data Wrangling
#####################################

inTraining<-sample(2,nrow(movie_df),prob = c(0.7,0.3),replace = T) 
train_set<-movie_df[inTraining==1,]
test_set<-movie_df[inTraining==2,]
nrow(train_set)

#1. Sampling Data
?subset
color_movies <- subset(movie_df$movie_title, movie_df$color == "Color" & movie_df$duration >200)
color_movies

#2. Filtering Data
movie_list <- movie_df[,c(2,12)]
movie_list

#3. Removing NULL values 
# represented by "NA"
#If output is > 0 then there are 'NA' value
sum(ifelse(is.na(movie_df$color),1,0))

#'NA' values or records are removed
movie_df_new <- na.omit(movie_df)
View(movie_df_new)

#4. Text Manipulation -- To remove special char "Â"
movie_df$movie_title <- gsub(pattern = " ", replacement = "", movie_df$movie_title)
View(movie_df)

#apply family in R
data <- matrix(c(1:10, 21:30), nrow = 5, ncol = 4)
data

apply(data, 2, mean)

#tapply #tapply splits the array based on specified data, usually factor levels and then applies the function to it.
library(datasets)
tapply(mtcars$wt, mtcars$cyl, mean)

#The tapply function first groups the cars together based on the number of cylinders they have, and then calculates the mean weight for each group.

#table command functionality 

## generate data for medical example
  clinical.trial <-
  data.frame(patient = 1:100,
             age = rnorm(100, mean = 60, sd = 6),
             treatment = gl(2, 50,
                            labels = c("Treatment", "Control")),
             center = sample(paste("Center", LETTERS[1:5]), 100, replace = TRUE))

#We want to know how many subjects are enrolled at each center in a clinical trial.

table(clinical.trial$center)

#We want to know how many subjects are under the age of 60 in a clinical trial.

## a logical vector is created and passed into table
table(clinical.trial$age < 60)

#dplyr package functionality

library(dplyr)
data(iris) # load the iris data

#step_1 %>% step_2 %>% step_3 %>% ... %>% step_n
iris2 <- as.data.frame(iris)

iris2 %>%
  filter(Petal.Length > 2) %>%
  group_by(Species) %>%
  summarise(mean(Petal.Length))

#####################################
#  Data Visualization
#####################################

#1. Strip Chart - Plots data in order
stripchart(movie_df$gross)

#2 Histogram - Plots the frequency
hist(movie_df$title_year)

#3 Box Plot
boxplot(movie_df$imdb_score)

#4 Scatter Plot
plot(movie_df$title_year, movie_df$director_facebook_likes)

#Some more visualization on the datasets

installed.packages("ggplot2")
library("ggplot2")

#mtcars dataset

data("mtcars")
summary(mtcars)

# Use data from data.frame
qplot(mpg,data = mtcars)
qplot(wt,data = mtcars)

qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = I("red"))

qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)

# Use different geoms
qplot(mpg, wt, data = mtcars, geom = "path")
qplot(factor(cyl), wt, data = mtcars, geom = c("boxplot"))
qplot(mpg, data = mtcars, geom = "dotplot")

#Transformations

plot(hist(mtcars$disp))
summary(mtcars$disp)

plot(hist(log(mtcars$disp)))
summary(log(mtcars$disp))

plot(hist(sqrt(mtcars$disp)))
summary(sqrt(mtcars$disp))

plot(hist(exp(mtcars$disp)))
summary(exp(mtcars$disp))

#Let us create multiple histograms on one plot
install.packages('gridExtra')
library(gridExtra)

a<-qplot(x=mtcars$disp,data=mtcars)
b<-qplot(x=log(mtcars$disp),data=mtcars)
c<-qplot(x=sqrt(mtcars$disp),data=mtcars)

grid.arrange(a,b,c,ncol=1)

