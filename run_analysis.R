R version 3.3.0
filesPath <- "C:/Users/owner/Documents/data/data/UCI HAR Dataset"
setwd(filesPath)

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

###Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

##Libraries used for data manipulation
library(dplyr)
library(data.table)
library(tidyr)
        
#Prepare data tables to make a dataset (training & test)
filesPath <- "C:/Users/owner/Documents/data/data/UCI HAR Dataset"

Subject_Training <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
Subject_Testing  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

Activity_Training <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
Activity_Testing  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

dat_Train <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dat_Test  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))



#1. Merging datasets to make a single dataset that comprises the "train" and "test" reqiures 
#a process of renaming variables in subject.txt and activity.txt files. In both files "v1" requires renaming. 
#Hence the merged Activitity and Subject files with "v1" acting as variables will be appropriately renamed as
# V1 = subject and v1 = "activityNum". These are critical identification variables.
 
subjectsMerge <- rbind(Subject_Training, Subject_Testing)
setnames(subjectsMerge, "V1", "subject")

activitiesMerge <- rbind(Activity_Training, Activity_Testing)
setnames(activitiesMerge, "V1", "activityNum")

#column names from the activity_labels file (created inadvance of Q2)
activity_labels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activity_labels, names(activity_labels), c("activityNum","activityName"))


#merge the basic "train" and "test" files
combined_data <- rbind(dat_Train, dat_Test)

# Name the variables in combined_data using the "feature" file
Add_Features <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(Add_Features, names(Add_Features), c("featureNum", "featureName"))
colnames(combined_data) <- Add_Features$featureName



#QUESTION 1, Answer:
#This creates the final process to creating a dataframe that combines both the test and train. 
combined_SubjectActivity<- cbind(subjectsMerge, activitiesMerge)
Final_datatable <- cbind(combined_SubjectActivity, combined_data)
str(Final_datatable)


#QUESTION 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Use "Add_Features" to extract variable names for the required mean and standard deviation measures
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",Add_Features$featureName,value=TRUE)


# Inorder calculate the mean and the standard deviation the renamed variables "subject","activityNum"
# need to be included.

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
Final_datatable<- subset(Final_datatable,select=dataFeaturesMeanStd) 
head(Final_datatable)
str(Final_datatable)

#QUESTION 3:Uses descriptive activity names to name the activities in the data set 
#Recall the activity_labels table created earlier! It is needed hear to add to final datatable.
Final_datatable <- merge(activity_labels, Final_datatable , by="activityNum", all.x=TRUE)
Final_datatable$activityName <- as.character(Final_datatable$activityName)

# create a datatable with descriptive activity names with mean measurements
Final_datatable$activityName <- as.character(Final_datatable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = Final_datatable, mean) 
Final_datatable <- tbl_df(arrange(dataAggr,subject,activityName))
str(Final_datatable)

#Question 4. Appropriately labels the data set with descriptive variable names
# "t" or "f"  represent time or frequency respectively.
#Body = related to body movement.
#Gravity = acceleration of gravity.
#Acc = accelerometer measurement
#Gyro = gyroscopic measurements
#Jerk = sudden movement acceleration
#Mag = magnitude of movement


names(Final_datatable)<-gsub("std()", "SD", names(Final_datatable))
names(Final_datatable)<-gsub("mean()", "MEAN", names(Final_datatable))
names(Final_datatable)<-gsub("^t", "time", names(Final_datatable))
names(Final_datatable)<-gsub("^f", "frequency", names(Final_datatable))
names(Final_datatable)<-gsub("Acc", "Accelerometer", names(Final_datatable))
names(Final_datatable)<-gsub("Gyro", "Gyroscope", names(Final_datatable))
names(Final_datatable)<-gsub("Mag", "Magnitude", names(Final_datatable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

head(str(Final_datatable),6)

#Question 5
write.table(Final_datatable, "TidyData.txt", row.name=FALSE)
