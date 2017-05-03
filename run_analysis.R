## This script was written by Patrick Devlin for the JHU Coursera "Getting and Cleaning Data" week 4 assignment
## This script uses the data found in the UCI HAR Dataset
## source:  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## For more information about this dataset contact: activityrecognition@smartlab.ws
##
##License:
##  ========
##  Use of this dataset in publications must be acknowledged by referencing the following publication [1] 
##
##[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
##
##This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
##
##Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

# load libraries used in this script
require(dplyr)
require(decoder)


# The first part of this script merges the training and test sets to one dataset

XtestData <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytestData <- read.table("./UCI HAR Dataset/test/y_test.txt")
XtrainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrainData <- read.table("./UCI HAR Dataset/train/y_train.txt")


features <- read.table("./UCI HAR Dataset/features.txt") #features are the variable names

# generate character vector of the features
features_character <- as.character(unlist(features[,2]))

## Rename Df columns to their respective features ASSIGNMENT REQUIREMENT 4 ##
colnames(XtestData) <- features_character
colnames(XtrainData) <- features_character

#Rename columns to "Activity" for better readibility/tidy-ness
ytestData <- rename(ytestData, Activity = V1)
ytrainData <- rename(ytrainData, Activity = V1)

# Add Activity column to its respective data frame
mergedTestData <- cbind(ytestData, XtestData)
mergedTrainData <- cbind(ytrainData, XtrainData)

## Combine the Test and Train data for ASSIGNMENT REQUIREMENT 1 ##
TestAndTrainDf <- rbind(mergedTestData, mergedTrainData)

# remove large intermediate Dfs from memory, force perform garbage collection
rm(mergedTestData, mergedTrainData)
gc()

## Select only Mean and Std observations ASSIGNMENT REQUIREMENT 2 ##
# first, make syntactically valid names using make.names so that dplyr select will work
valid_column_names <- make.names(names=names(TestAndTrainDf), unique=TRUE, allow_ = TRUE)
#rename DF with the valid names
names(TestAndTrainDf) <- valid_column_names
# Select only Mean and Std obervations ASSIGNMENT REQUIREMENT 2
MeanStOnly <- select(TestAndTrainDf, Activity, matches("mean|std"))

## Replace Activty column labels with activity name from activity_labels.txt ASSIGNMENT REQUIREMENT 3 ##

activity_labels <- c(WALKING = 1, WALKING_UPSTAIRS = 2, WALKING_DOWNSTAIRS = 3, SITTING = 4, STANDING =5, LAYING = 6)
MeanStOnly$Activity <- decode(MeanStOnly$Activity, activity_labels)

## ASSIGNMENT REQUIREMENT 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# read in subject labels and append to beginning of MeanStOnly data frame
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")##
subject <- rbind(subject_test, subject_train)
BySubject <- cbind(subject, MeanStOnly)

#rename Subject column
BySubject <- rename(BySubject, Subject = V1)

#use dplyr package to group BySubject data frame by Subject and Activity, then calculate the mean of each variable
BySubject <- BySubject %>%
  group_by(Subject, Activity) %>%
  summarise_each(funs(mean))
  
#Dataframe BySubject fulfills assignment requirement 5

#write tidy datasets to file
write.table(MeanStOnly, file = "MeanStOnly.txt", row.names = FALSE)
write.table(BySubject, file = "BySubject.txt", row.names = FALSE)