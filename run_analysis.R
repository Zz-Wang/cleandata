###Coursera Getting and Cleaning Data Project
#setwd("~/Documents/R/UCI_HAR_Dataset/")

#task 1 Merges the training and the test sets to create one data set
trainData <- read.table("./train/X_train.txt")
testData <- read.table("./test/X_test.txt")
myData <- rbind(trainData, testData)

trainLabel <- read.table("./train/y_train.txt")
testLabel <- read.table("./test/y_test.txt") 
myLabel <- rbind(trainLabel, testLabel)

#task 2 Extracts only the measurements on the mean and 
#standard deviation for each measurement
features <- read.table("features.txt")
indices_of_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
myData <- myData[, indices_of_features]
names(myData) <- features[indices_of_good_features, 2]
names(myData) <- gsub("\\(|\\)", "", names(myData))
names(myData) <- tolower(names(myData))

#task 3 Uses descriptive activity names to name the activity in the data set
activity <- read.table("./activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
myLabel[,1] = activity[myLabel[,1], 2]
names(myLabel) <- "activity"

#task 4 Appropriately labels the data set with descriptive variable names. 
trainSubject <- read.table("./train/subject_train.txt")
testSubject <- read.table("./test/subject_test.txt")
mySubject <- rbind(trainSubject, testSubject)

names(mySubject) <-  "subject"
cleanedData <- cbind(mySubject, myLabel, myData)
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

#task 5 Creates a second, independent tidy data set with the average of 
#each variable for each activity and each subject. 
numSubjects = length(unique(mySubject)[,1])
numActivity = length(activity[,1])
numCols = dim(cleanedData)[2]
result = cleanedData[1:(numSubjects*numActivity), ]
uniqueSubjects = unique(mySubject)[,1]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivity) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleanedData[cleanedData$subject==s & cleanedData$activity==activity[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt", row.name=FALSE)
