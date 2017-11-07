#Clear up the environment
rm(list=ls())

#Read the file
task_r <- read.csv(file.choose(), header=T)

#Dimension and Summary
dim(task_r)   # [30000,8]
str(task_r)
View(task_r)
summary(task_r)

#Check the missing value pattern
install.packages("mice")
library(mice)
md.pattern(task_r) #No missing data in the file



#Check the unique recommendation ID's
length(unique(task_r$recommendation_id))  #2100 unique recommendation sets


#Mean and median number of taskers

#Split taskers by recommendation groups and calculate no. of taskers in each recommendation using list
q<-lapply(split(task_r$tasker_id, task_r$recommendation_id), function(x) length(x))

#convert the list to data frame to view summary
p<-do.call(rbind, lapply(q, data.frame, stringsAsFactors=FALSE))
summary(p)   #mean/average no of taksers= 14.29 #Median number of taskers=15


#Check Unique Taskers
length(unique(task_r$tasker_id))          #830 unique taskers

#Most shown tasker
tail(sort(table(task_r$tasker_id)), 5) #1014508755 shown 608 times
   
#Least shown tasker
least_shown<-as.data.frame(sort(table(task_r$tasker_id)))#1006690425
least_shown[least_shown$Freq==1,] #68 taskers which are shown least(only once), 
#1006690425 has the smallest tasker ID and 1014926743 has the largest ID
#ties can be broken in the ID order or the given criteria



#most and least hired tasker
install.packages("dplyr")
library(dplyr)
tasker_summary<-task_r %>% 
  group_by(tasker_id) %>% 
  summarise(hired = sum(hired)) %>% 
  ungroup %>%
  arrange(-hired, tasker_id) 
head(tasker_summary,2)  #Tasker 1012043028 has been hired the most which is 59 times
View(tasker_summary)
#Least hired taskers
tasker_summary[tasker_summary$hired==0,]
#508 taskers have been hired 0 times. 
#Ties can be broken by sorting tasker_id. #Smallest tasker_id with 0 hiring= " 1006646767 "
tail(tasker_summary[tasker_summary$hired==0,],1)
#Largest tasker_id with 0 hiring = " 1015000442 "

#Calculate conversion rates

#Count the no. of times a tasker is recommended
tasker_recommended<-count(task_r, tasker_id)
View(tasker_recommended)

#Merge the data frame containing the no. of times a tasker is hired, and recommended
conversion<- merge(tasker_recommended, tasker_summary, all.x=TRUE)
View(conversion)
conversion$conversion_rate <- 100*(conversion$hired/conversion$n)
View(conversion)

#Taskers having 100% conversion rate
nrow(conversion[conversion$conversion_rate==100,])  #6 taskers with 100 % convrsion rate

conversion[conversion$conversion_rate==100,]
# 1007480912    1008094420    1008861741     1011985968   1012369686 1014478773, 6 taskers with 100%
#conversion rate

# Question: Can all taskers have 100% conversion rate?
# Answe: We have 830 unique taskers and 2100 unique recommendation ID's
#which means a tasker is recommended more than once and also at the same time.
#To achieve a 100% conversion rate a recommendation_id can have only one tasker, because 
#all the taskers shoud always be hired, which is practically not feasible
#as we need to provide a set of taskers to the person(recommendation_id) who is going to hire them
#Only few taskers can have a conversion rate of 100%
# Demand and Supply Logic: 1 demand, several options for supply, only 1 supply would be chosen

#Average position of the tasker who is hired
avg_position<-task_r[task_r$hired==1,]  #set of taskers who are hired
avg_position %>% 
  group_by(category) %>% 
  summarise(position = mean(position)) %>% 
  ungroup %>%
  arrange(category, position) 
#category              position
#1 Furniture Assembly 3.611888
#2           Mounting 4.596085
#3        Moving Help 4.145359
 


library(dplyr)
avg_position %>% 
  group_by(category) %>% 
  summarise(hourly_rate = mean(hourly_rate), num_completed_tasks=mean(num_completed_tasks)) %>% 
  ungroup %>%
  arrange(hourly_rate, num_completed_tasks,category)
#           category    hourly_rate      num_completed_tasks
#1 Furniture Assembly    38.70105            249.0210
#2           Mounting    50.15480            284.0961
#3        Moving Help    63.01226            273.8827


# hourly rates vs hiring
boxplot(hourly_rate~hired, data=task_r, notch=TRUE, 
             col=(c("gold","darkgreen")),
            main="Hourly rate plot", xlab="Hired")  #Maximum and third quantile of hourly rates
#for taskers who are hired is quite less than people who aren't hired


# Hourly rates to maximize the probability to be hired

#Density function
plot(density(task_r$hourly_rate),main= "Density function of hourly rate") #Highly skewed data
polygon(density(task_r$hourly_rate),col="pink", border="black")

#Logistic Regression
install.packages("caret")
library(caret)
#dummy <- dummyVars(~ category, task_r)
#dummy_df <- as.data.frame(predict(dummy, task_r))
#dummy_df <- data.frame(task_r[c("recommendation_id","created_at","hourly_rate","num_completed_tasks","position", "tasker_id"
#                                     )],dummy_df, task_r["hired"] )
#logRegtask_r <- glm(hired~ hourly_rate+position+num_completed_tasks+category.Furniture.Assembly+category.Mounting+category.Moving.Help,data= dummy_df ,family=binomial(link='logit'))
logRegtask_r <- glm(hired~ hourly_rate+position+num_completed_tasks+category,data= task_r ,family=binomial(link='logit'))
summary(logRegtask_r)

#Considering a combination of taskerID and recommendation ID is a unique key

#logit(p) =log(p/(1-p))=B0+B1*hourly_rate+B2*position+B3*num_completed_tasks
#logit(p)=-1.053-0.006752*hourly_rate-0.2641*position+0.001036*num_completed_tasks
#The coefficient for hourly rate says that, holding position and num_completed tasks at a fixed value,
#we will see 10.1% decrease in the odds of getting hired 
#for a one-unit increase in hourly rate since exp(0.006752) = 1.006775



plot(task_r$hourly_rate, logRegtask_r$fitted, pch=19, col="blue", xlab="Hourly Rate", ylab="Prob of getting hired")
exp(logRegtask_r$coefficients)
exp(confint(logRegtask_r))
#task_r$s <- predict(logRegtask_r,type='response')
#sum(with(task_r,2*(s-hired)))
#sum(with(task_r,2*(s-hired)*hourly_rate))



# Checking if it is okay to not include Data and time
#Too many recommendation ID and tasker ID to dummy encode, so subsetting by categories
category_task<-task_r[task_r$category=="Furniture Assembly",]
length(unique(category_task$recommendation_id))#702
length(unique(category_task$tasker_id)) # 514

#Converting the time stapms into days and range of hours(Some range of hours will have
#more demand, so hourly rates will depend on that, also just abstracting the date
# as this data is for only month of September
library(lubridate)
category_task$Date <- as.Date(category_task$created_at)
category_task$Date<-format(category_task$Date, "%d")
category_task$Time <- format(as.POSIXct(category_task$created_at) ,format = "%H:%M:%S") 
category_task$hour<-hour(as.POSIXct(category_task$created_at))
category_task<-category_task[, c(-2,-10)]
View(category_task)


#Encoding the hours by ranges
#Range W= [12:00 am to 6am)
#Range X= [6am to 12pm)
#Range Y= [12pm to 6pm)
#Range Z= [6pm to 12:00am)
i<- category_task[category_task$hour==0|category_task$hour==1|category_task$hour==2|category_task$hour==3|
              category_task$hour==4|category_task$hour==5,]
i$hour <- "X"
j<- category_task[category_task$hour==6|category_task$hour==7|category_task$hour==8|category_task$hour==9|
                    category_task$hour==10|category_task$hour==11,]
j$hour <- "Y"
k<- category_task[category_task$hour==12|category_task$hour==13|category_task$hour==14|category_task$hour==15|
                    category_task$hour==16|category_task$hour==17,]
k$hour <- "Z"
l<- category_task[category_task$hour==18|category_task$hour==19|category_task$hour==20|category_task$hour==21|
                    category_task$hour==22|category_task$hour==23,]
l$hour <- "M"
total<-rbind(i,j,k,l)
View(total)
install.packages("dummies")
library(dummies)
logRegtask_r_dummy <- glm(hired~ hourly_rate+position+num_completed_tasks+dummy(category)+dummy(hour)+dummy(Date),data= category_task ,family=binomial(link='logit'))

summary(logRegtask_r_dummy)
#From this model found out that date, time and categories are not a significant variable

logRegtask_r_dummy <- glm(hired~ hourly_rate+position+num_completed_tasks+dummy(category)+
                            dummy(hour)+dummy(Date)+dummy(tasker_id)+dummy(recommendation_id),
                          data= category_task ,family=binomial(link='logit'))
# 
install.packages("knitr")
install.packages("markdown")