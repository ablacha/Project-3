#Coursera project for the course Getting and Cleaning Data

# Set working directory
setwd("C:/Users/User/Documents/R/")

# Download and unzip data
# 1. Donwload data from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" and save
# to the local drive
# 2. Unzip the file to the directory "./data" so that all files are in "./data/UCI HAR Dataset/"

#load data to data frames
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
x_train <- read.table("./data/UCI HAR Dataset/train/x_train.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
x_test <- read.table("./data/UCI HAR Dataset/test/x_test.txt")

subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
features <- read.table("./data/UCI HAR Dataset/features.txt")
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

# Objective 4. Appropriately labels the data set with descriptive variable names
# give column names to data frames
names(x_train) <- features$V2
names(x_test) <- features$V2
names(y_train) <- c("Labels")
names(y_test) <- c("Labels")
names(activity_labels) <- c("Labels", "Activity")
names(subject_train) <- c("Subject")
names(subject_test) <- c("Subject")

# combine row labels with subjects and data observations
x_train <- cbind(y_train, subject_train, x_train)
x_test <- cbind(y_test, subject_test, x_test)

# Obejective 1. Merges the training and the test sets to create one data set.
#join both data sets
x_joint = rbind(x_train, x_test)

# Objective 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# define a vector of items based on std or mean 
good <- (grepl("Labels", names(x_joint), ignore.case = TRUE) | grepl("Subject", names(x_joint), ignore.case = TRUE) | grepl("mean", names(x_joint), ignore.case = TRUE) | grepl("std", names(x_joint), ignore.case = TRUE)) & !grepl("meanFreq", names(x_joint), ignore.case = TRUE) 
x_joint_mean_std <- x_joint[, good]

# Objective 3. Uses descriptive activity names to name the activities in the data set
# give descriptive names to activity lables

# merge joint data set (filted on mean and std) with activity labels based on Labels
mergedData = merge(x_joint_mean_std, activity_labels, by.x="Labels", by.y = "Labels", all = FALSE)

# Objective 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
install.packages('reshape2',type='source')
library(reshape2)

#get ids for melt function (drop Label, Activity and Subject)
ids = names(mergedData)[4:ncol(mergedData)-1]

melted = melt(mergedData, id = c("Subject", "Activity"), measure.vars = ids)
# calculate variable containing averages per each varaible (in columns) and each subject and activity
Subj_Act_Data = dcast(melted, Subject + Activity ~ variable, mean)

# define row labels
# rowLabels = paste("Subject", Subj_Act_Data$Subject, "-", Subj_Act_Data$Activity)
# rownames(Subj_Act_Data) <- rowLabels

# Drop ID columns
# Subj_Act_Data = Subj_Act_Data[3:ncol(Subj_Act_Data)]

# redefine column names (leaving ID nams unchanged) and adding a pre-fix Ave to other names
colVarLabels = paste("Ave", colnames(Subj_Act_Data)[3:ncol(Subj_Act_Data)])
colnames(Subj_Act_Data)[3:ncol(Subj_Act_Data)] <- colVarLabels

#write the result
write.table(Subj_Act_Data, file = "./data/UCI HAR Dataset/result.txt", row.name=FALSE)


