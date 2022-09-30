library(tidyverse)
library(dplyr)
rm(list=ls())
setwd("C:/Users/Maryam.Husain/Documents")

#download given data 
if(!file.exists("./project")){dir.create("./project")}
projUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(projUrl,destfile="./project/coursera.zip")

unzip(zipfile="./project/coursera.zip",exdir="./project")

#Project instructions:
#You should create one R script called run_analysis.R that does the following:

#1)Merges the training and the test sets to create one data set.
#2)Extracts only the measurements on the mean and standard deviation for each measurement. 
#3)Uses descriptive activity names to name the activities in the data set
#4)Appropriately labels the data set with descriptive variable names. 
#5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#1) 
features=read.table('./project/UCI HAR Dataset/features.txt',col.names = c("n","functions"))
actLabels = read.table('./project/UCI HAR Dataset/activity_labels.txt',col.names = c("codes", "activity"))

Xtrain=read.table("./project/UCI HAR Dataset/train/X_train.txt",col.names = features$functions)
Ytrain=read.table("./project/UCI HAR Dataset/train/y_train.txt",col.names = "code")
sub_train=read.table("./project/UCI HAR Dataset/train/subject_train.txt",col.names = "subj")

Xtest=read.table("./project/UCI HAR Dataset/test/X_test.txt",col.names = features$functions)
Ytest=read.table("./project/UCI HAR Dataset/test/y_test.txt",col.names = "code")
sub_test=read.table("./project/UCI HAR Dataset/test/subject_test.txt",col.names = "subj")

x=rbind(Xtrain, Xtest)
y=rbind(Ytrain, Ytest)

rm(Xtest,Xtrain,Ytrain,Ytest)
Subj=rbind(sub_train, sub_test)
merged_dat=cbind(Subj, y, x)

#2)
dat=merged_dat[,grepl("mean.|std.|subj|code",colnames(merged_dat))]

#3)
dat$code <- activityLabels[dat$code, 2]

#4)
dat=dat%>%rename(activity=code)
names(dat)=gsub("Acc", "Accelerometer", names(dat))
names(dat)=gsub("Gyro", "Gyroscope", names(dat))
names(dat)=gsub("BodyBody", "Body", names(dat))
names(dat)=gsub("Mag", "Magnitude", names(dat))
names(dat)=gsub("^t", "Time", names(dat))
names(dat)=gsub("^f", "Frequency", names(dat))
names(dat)=gsub("tBody", "TimeBody", names(dat))
names(dat)=gsub("-mean()", "Mean", names(dat), ignore.case = TRUE)
names(dat)=gsub("-std()", "STD", names(dat), ignore.case = TRUE)
names(dat)=gsub("-freq()", "Frequency", names(dat), ignore.case = TRUE)
names(dat)=gsub("angle", "Angle", names(dat))
names(dat)=gsub("gravity", "Gravity", names(dat))

#5)
dat_final=dat%>%
  group_by(subj, activity)%>%
  summarise_all(funs(mean))

#save final dataset
write.table(dat_final, "FinalData.txt", row.name=FALSE)




