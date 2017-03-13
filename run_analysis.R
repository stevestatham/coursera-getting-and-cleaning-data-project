#
#Review criteria
# 1. The submitted data set is tidy.
# 2. The Github repo contains the required scripts.
# 3. GitHub contains a code book that modifies and updates the available codebooks with the data to indicate 
#       all the variables and summaries calculated, along with units, and any other relevant information.
# 4. The README that explains the analysis files is clear and understandable.
# 5. The work submitted for this project is the work of the student who submitted it.

#Getting and Cleaning Data Course Project
#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
#The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 
#     1) a tidy data set as described below 
#     2) a link to a Github repository with your script for performing the analysis, and 
#     3) a code book that describes the variables, the data, and any transformations or work that you performed 
#         to clean up the data called CodeBook.md. 
#         You should also include a README.md in the repo with your scripts. 
#         This repo explains how all of the scripts work and how they are connected.

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article. 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
#The data linked to from the course website represent data collected from the accelerometers from the 
#Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

#Data Description
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#Data Source
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

#You should create one R script called run_analysis.R that does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the dActs in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Sets the Working Directory

setwd("C:/Users/Steve/Desktop/Coursera/Data Science Course/GettingAndCleaningData/UCI HAR Dataset/")

# 1. Merges the training and the test sets to create one data set.

xtrain <- read.table("./train/X_train.txt")
xtest <- read.table("./test/X_test.txt")
xMerged <- rbind(xtrain, xtest)

strain <- read.table("./train/subject_train.txt")
stest <- read.table("./test/subject_test.txt")
sMerged <- rbind(strain, stest)

ytrain <- read.table("./train/y_train.txt")
ytest <- read.table("./test/y_test.txt")
yMerged <- rbind(ytrain, ytest)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("./features.txt")                             #Load the features
good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])       #extract means and std dev
xMerged <- xMerged[, good_features]
names(xMerged) <- features[good_features, 2]
names(xMerged) <- gsub("\\(|\\)", "", names(xMerged))
names(xMerged) <- tolower(names(xMerged))

# 3. Uses descriptive activity names to name the activities in the data set

dActs <- read.table("./activity_labels.txt")                         #Load the activities
dActs[, 2] = gsub("_", "", tolower(as.character(dActs[, 2]))) 
yMerged[,1] = dActs[yMerged[,1], 2]

# 4. Appropriately labels the data set with descriptive activity names.

names(yMerged) <- "activity"
names(sMerged) <- "subject"
merged <- cbind(sMerged, yMerged, xMerged)

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uSubjects = unique(sMerged)[,1]
nSubjects = length(unique(sMerged)[,1])
nActivities = length(dActs[,1])
nCols = dim(merged)[2]
result = merged[1:(nSubjects*nActivities), ]

row = 1
for (i in 1:nSubjects) {
  for (j in 1:nActivities) {
    result[row, 1] = uSubjects[i]
    result[row, 2] = dActs[j, 2]
    tmp <- merged[merged$subject == i & merged$activity == dActs[j, 2], ]
    result[row, 3:nCols] <- colMeans(tmp[, 3:nCols])
    row = row+1
  }
}
write.table(result, "tidy.txt")