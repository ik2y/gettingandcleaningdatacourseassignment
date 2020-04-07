library(data.table)
library(dplyr)

## Part 1
## Read supporting data
featureNames <- read.table("D:/Users/fifikri/Documents/Assignment Week 4/features.txt")
activityLabels <- read.table("D:/Users/fifikri/Documents/Assignment Week 4/activity_labels.txt", header = FALSE)


## Read training data
subjectTrain <- read.table("D:/Users/fifikri/Documents/Assignment Week 4/subject_train.txt", header = FALSE)
activityTrain <- read.table("D:/Users/fifikri/Documents/Assignment Week 4/y_train.txt", header = FALSE)
featuresTrain <- read.table("D:/Users/fifikri/Documents/Assignment Week 4/X_train.txt", header = FALSE)

## Read test data
subjectTest <- read.table("D:/Users/fifikri/Documents/Assignment Week 4/subject_test.txt", header = FALSE)
activityTest <- read.table("D:/Users/fifikri/Documents/Assignment Week 4/y_test.txt", header = FALSE)
featuresTest <- read.table("D:/Users/fifikri/Documents/Assignment Week 4/X_test.txt", header = FALSE)

## Merge training and test data
subject <- rbind(subjectTrain,subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

## Column names
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

## Merge all data
completeData <- cbind(features,activity,subject)

## Part 2
## Extract column means and standard deviation
columnsMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case = TRUE)
requiredColumns <- c(columnsMeanSTD, 562, 563)
extractData <- completeData[,requiredColumns]

## Part 3
## Uses descriptive activity names to name the activities in the data set
extractData$Activity <- as.character(extractData$Activity)
  for (i in 1:6) {
    extractData$Activity[extractData$Activity ==i] <- as.character(activityLabels[i,2])

  }

extractData$Activity <- as.factor(extractData$Activity)

## Part 4
## Appropriately labels the data set with descriptive variable names.

names(extractData)<-gsub("Acc", "Accelerometer", names(extractData))
names(extractData)<-gsub("Gyro", "Gyroscope", names(extractData))
names(extractData)<-gsub("BodyBody", "Body", names(extractData))
names(extractData)<-gsub("Mag", "Magnitude", names(extractData))
names(extractData)<-gsub("^t", "Time", names(extractData))
names(extractData)<-gsub("^f", "Frequency", names(extractData))
names(extractData)<-gsub("tBody", "TimeBody", names(extractData))
names(extractData)<-gsub("-mean()", "Mean", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("-std()", "STD", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("-freq()", "Frequency", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("angle", "Angle", names(extractData))
names(extractData)<-gsub("gravity", "Gravity", names(extractData))

## Part 5
## From the data set in Part 4, creates a second, 
## independent tidy data set with the average of each variable for each activity and each subject.

extractData$Subject <- as.factor(extractData$Subject)
extractData <- data.table(extractData)

tidyData <- aggregate(. ~Subject + Activity, extractData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)


