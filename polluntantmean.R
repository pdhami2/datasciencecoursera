# Code for the Coursera Course
# Code Owner: Priyanka Dhamija
# Clear up the environment
rm(list=ls())

#set working directory
setwd("C:/Users/priya/Documents/Priyanka/Self Learning/Coursera/R Programing/specdata/")

#FUNCTION 1: to read data from multiple files in a directory
pollutantmean<- function(directory, pollutant, id = 1:332){
  if(grep("specdata",directory)==1)
  {
    directory<-("C:/Users/priya/Documents/Priyanka/Self Learning/Coursera/R Programing/specdata/")
  }
  mean_file<-c()
  allfiles<-as.character(list.files(directory))
  filepath<- paste(directory, allfiles, sep="")
  for(i in id){
    current_file<-read.csv(filepath[i],header = T, sep = ",")
    mean_file<-c(mean_file,current_file[!is.na(current_file[, pollutant]), pollutant])
  }
  finalmean<-mean(mean_file)
  return(finalmean)
}

#test
pollutantmean("specdata", "sulfate", 1:10) #4.06
pollutantmean("specdata", "nitrate", 70:72)  #1.706
pollutantmean("specdata", "nitrate", 23)  #1.280


# FUNCTION 2: to read a directory full of files and return a dataframe with name and 
#number of complete cases
install.packages("dplR")
complete<-function(directory, id=1:332){
  if(grep("specdata",directory)==1)
  {
    directory<-("C:/Users/priya/Documents/Priyanka/Self Learning/Coursera/R Programing/specdata/")
  }
  nobs<-c()
  allfiles<-as.character(list.files(directory))
  filepath<- paste(directory, allfiles, sep="")
  for(i in id){
    current_file<-read.csv(filepath[i],header = T, sep = ",")
    nobs<- c(nobs,sum(complete.cases(current_file)))
  }
  df<-data.frame(id, nobs)
  print(df)
}

# Testing 
complete("specdata", 30:25)

#FUNCTION 3 : Corerelation between variables of set of files in a directory
corr <- function(directory, threshold = 0) {
  if(grep("specdata",directory)==1)
  {
    directory<-("C:/Users/priya/Documents/Priyanka/Self Learning/Coursera/R Programing/specdata/")
  }
  
  observations <- complete("C:/Users/priya/Documents/Priyanka/Self Learning/Coursera/R Programing/specdata/")
  filtered_observations = subset(observations,observations$nobs > threshold)
  
  file_list <- list.files(directory)
  correlation <- vector()
  
  # Each Id filtered observations:
  for (i in filtered_observations$id) {
    file_dir <- paste(directory,file_list[i],sep="")
    file_data <- read.csv(file_dir)
    # remove NA's
    file_data <- subset(file_data,complete.cases(file_data))        
    # Calculate the cor, Accumulate it in correlation vector
    correlation <- c(correlation,cor(file_data$nitrate,file_data$sulfate))    
  }
  # output correlation 
  correlation
}
