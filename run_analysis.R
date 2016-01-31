##
## Week 4 project
##
## (R) Juan P. Salvador, 30JAN2016
##
##

##
## Fisrt steps
##
## The data have been downloaded to "dataset" directory
##
## set working directory in my laptop !
## setwd("/Users/jp/Desktop/coursera.org/Getting and cleaning data/w4material/gacdW4project")
## 

## get features labels
labels <- read.csv("./dataset/features.txt", header=FALSE, sep=" ")[,2]
labels <- gsub("\\(\\)", "", labels)    ## fix features names deleting the "()" symbol

## get activity labels
activity_labels <- read.csv("./dataset/activity_labels.txt", header=FALSE, sep=" ")


##
##    1.- merge two datasets
##
test <- read.fwf("./dataset/test/X_test.txt",widths=rep(16,561),header=FALSE)      ## TEST
train <- read.fwf("./dataset/train/X_train.txt",widths=rep(16,561),header=FALSE)   ## TRAIN
testAndTrain <- rbind(test, train)
write.csv(testAndTrain, "Q1-testAndTrain.csv", row.names=FALSE)
##
##    Result for 1.- is in "Q1-testAndTrain.csv" file
##


##
##    2.- Extracts only the measurements on the mean and standard deviation for each measurement
##
meanAndStdCols <- grep("mean|std",labels)
meanAndStdMeasures <- testAndTrain[,meanAndStdCols]
colnames(meanAndStdMeasures) <- labels[grep("mean|std",labels)]
write.csv(meanAndStdMeasures, "Q2-meanAndStdMeasures.csv", row.names=FALSE)
##
##    Result for 2.- is in "Q2-meanAndStdMeasures.csv" file
##


##
##    3.- Uses descriptive activity names to name the activities in the data set
##
activity_test <- read.csv("./dataset/test/y_test.txt",header=FALSE)
activity_train <- read.csv("./dataset/train/y_train.txt",header=FALSE)
activityCodes <- rbind(activity_test, activity_train)
colnames(activityCodes)[1] <- c("activityCode")
colnames(activity_labels)<- c("activityCode","activity")
activityCodes$activity <- activity_labels$activity[match(activityCodes$activityCode, activity_labels$activityCode)]
write.csv(activityCodes, "Q3-activity.csv", row.names=FALSE)
##
##    Result for 3.- is in "Q3-activity.csv" file
##

##
## 4.- Appropriately labels the data set with descriptive variable names.
##
## set colnames to testAndTrain
colnames(testAndTrain) <- labels
## build subject column, then bind to train data frame
subject_test <- read.csv("./dataset/test/subject_test.txt",header=FALSE)
subject_train <- read.csv("./dataset/train/subject_train.txt",header=FALSE)
subject <- rbind(subject_test, subject_train)
colnames(subject) <- c("subject")
## attach sibject to testAndTrain
testAndTRainBySubject <- cbind(subject, testAndTrain)
## attach activity to testAndTrain
testAndTRainByActivityAndSubject <- cbind(activityCodes, testAndTRainBySubject)
## remove activityCode column
testAndTRainByActivityAndSubject$activityCode <- NULL  
## save results
write.csv(testAndTRainByActivityAndSubject, "Q4-testAndTRainWithActivityAndSubject.csv", row.names=FALSE)
##
##    Result for 4.- is in "Q4-testAndTRainWithActivityAndSubject.csv" file
##

#
##    Result for 5.- is in "Q5-testAndTRainWithActivityAndSubject.csv" file
##
##    5.- From the data set in step 4, creates a second, independent 
##        tidy data set with the average of each variable for each
##        activity and each subject
testAndTRainByActivityAndSubjectOrdered <- 
          testAndTRainByActivityAndSubject[with(testAndTRainByActivityAndSubject, order(activity, subject)), ]
testAndTrainAgg <- aggregate(testAndTRainByActivityAndSubjectOrdered,
                             by=list(activity = testAndTRainByActivityAndSubjectOrdered$activity,
                             subject = testAndTRainByActivityAndSubjectOrdered$subject),
                             FUN=mean,
                             na.rm=TRUE)
## There were 50 or more warnings (use warnings() to see the first 50)
write.csv(testAndTrainAgg, "Q5-testAndTrainAgg.csv", row.names=FALSE)
##
##    Result for 5.- is in "Q5-testAndTRainWithActivityAndSubject.csv" file
##
  
## to update github
## git commit -m "some useful mesage . . ."
## git push -u origin master
