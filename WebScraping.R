rm(list=ls())
if(!file.exists("./data")){dir.create("./data")}

#Reading data from web
fileUrl<-"https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv", method="curl")

#Lower Case
file<-read.csv(file.choose(), header=T)
View(file)
toupper(colnames(file))

#If the columns have a dot
splitNames<-strsplit(names(file),"\\.")
splitNames[5]

mylist<- list(letters=c("A","b","c"),numbers=1:3, matrix(1:25,ncol=5))

#Code to download data on Amazon's stock price
install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
View(amzn)
library(lubridate)

#Extracting no. of days with given year and weekday
category_task <- as.Date(sampleTimes)
sum(year(category_task)==2012) #250
sum(weekdays(category_task[year(category_task)==2012])=="Monday") #47

#Merging two data sets and matching the countries for which the end of the 
#fiscal year is available, finding how many end in June
gdp<-read.csv(file.choose(), header=TRUE)
View(gdp)
gdp<-gdp[5:235,]
education<-read.csv(file.choose(), header=TRUE)
mean(as.numeric(gsub(",", "", gdp$X.3)), na.rm = TRUE)

#count the number of countries whose name begins with "United"
grep("^United", gdp$X.2)

