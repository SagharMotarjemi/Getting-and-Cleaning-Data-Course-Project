getwd()
path <- setwd("/Users/ramin/Desktop/Get&CleD")

# Code --------------------------------------------------------------------


### Loading the required library
library(dplyr)
library(data.table)

### Reading the supporting metadata tables
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

### Reading the training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

### Reading the test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

### Merging the training with test to create one dataset based on rows
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

### Giving names to the colums
colnames(features) <- t(featureNames[2])

### Merging the data based on columns
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

### Extracting column indices with either mean or std 
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

### Adding activity and subject columns to the list and checking the data dimension 
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

### Checking the extraxted data dimensions
extractedData <- completeData[,requiredColumns]
dim(extractedData)

### Changing the activity field in extractedData from numeric to character.
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

### Now converting the activity column of the extracted data to factor
extractedData$Activity <- as.factor(extractedData$Activity)

### Now giving proper and informative names to the extracted data
names(extractedData)
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

### Finally, creating a second, independent tidy data set with the average of each variable for each activity and each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
str(tidyData)

### Extracting labels from the tidy data and creating a codebook
library(tibble)
library(sjlabelled)
simple_codebook <- enframe(get_label(tidyData))
colnames(simple_codebook) <- c("variable", "value")

### Store the results in a text file
write.table(simple_codebook, file = "CodeBook.md", row.names = FALSE)

