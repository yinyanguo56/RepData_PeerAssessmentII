
## create data folder
if(!file.exists("./data")){
  dir.create("data")
}

## download url and destination file
file.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
file.dest <- 'data/UCI_HAR_Dataset.zip'

## download from the URL
download.file(file.url, file.dest)

## unzip the file
if (!file.exists("data/UCI HAR Dataset")) { 
  unzip(file.dest,exdir="./data") 
}

## read in test data sets
subject_test<-  read.table(file.path("./data/UCI HAR Dataset/test/subject_test.txt"))
names(subject_test)[1] <-  "subject"

activity_test<-  read.table(file.path("./data/UCI HAR Dataset/test/y_test.txt"), sep="")
names(activity_test)[1] <- "activity"

feature_test<-  read.table(file.path("./data/UCI HAR Dataset/test/X_test.txt"), sep="")

## combine subject_test,activity_test, feature_test
test <- cbind(subject_test,activity_test,feature_test)


## read in train data sets
subject_train<-  read.table(file.path("./data/UCI HAR Dataset/train/subject_train.txt"))
names(subject_train)[1] <-  "subject"

activity_train<-  read.table(file.path("./data/UCI HAR Dataset/train/y_train.txt"), sep="")
names(activity_train)[1] <- "activity"

feature_train<-  read.table(file.path("./data/UCI HAR Dataset/train/X_train.txt"), sep="")

## combine subject_train,activityID_train, feature_train
train <- cbind(subject_train,activity_train,feature_train)

## combine test and train
test_train <- rbind(test,train)

## Extracts only the measurements on the mean() and standard deviation std()
## read in feature names
featureName<-  read.table(file.path("./data/UCI HAR Dataset/features.txt"))

meanStdFeature <- grep("mean\\(\\)*|.std\\(\\)*",featureName$V2)
test_train_featureonly <- test_train[,-c(1,2)]
featureWanted <- test_train_featureonly[,c(meanStdFeature)]

##Uses descriptive activity names to name the activities in the data set
subject_activity <- test_train[,c(1,2)]
activityLabels<-  read.table(file.path("./data/UCI HAR Dataset/activity_labels.txt"))
subject_activity$activity <- factor(subject_activity$activity, levels = activityLabels[,1], labels = activityLabels[,2])

## subject + activity + test_train_featureonly
featureWanted <- cbind(subject_activity, featureWanted)

## Appropriately labels the data set with descriptive variable names.
## meanstdName is dataframe of full discription about wantedFeature
meanstdName <- featureName[meanStdFeature,]
names(meanstdName)[1:2] <-  c("Ith_feature","FeatureName")

## add one column of featurewanted  name for editing
meanstdName$FeatureName_short <- meanstdName[,2]

 ## change -std() to Std in featurewanted  name
meanstdName$FeatureName_short  <- gsub('-std\\(\\)', 'Std', meanstdName$FeatureName_short)

## change -mean() to Mean in featurewanted  name
meanstdName$FeatureName_short  <- gsub('-mean\\(\\)', 'Mean', meanstdName$FeatureName_short)

## change "-" to "_" in featurewanted  name
meanstdName$FeatureName_short <- gsub('-', '', meanstdName$FeatureName_short)

## change columne as charactor vector
columnName <- as.character(meanstdName$FeatureName_short)

## rename featureWanted name in feature table
names(featureWanted)[3:68] <-  c(columnName)


## From the data set in step 4, creates a second, independent tidy data set with
## the average of each variable for each activity and each subject.
library(reshape2)
featureWanted.melted <- melt(featureWanted, id = c("subject", "activity"))
featureWanted.mean <- dcast(featureWanted.melted, subject + activity ~ variable, mean)

## Write the results
if(!file.exists("./result")){
  dir.create("result")
}

library(gdata)
write.fwf(featureWanted, "./result/featureWanted.txt",  quote = FALSE,)
write.table(featureWanted.mean, "./result/featureWanted_mean.txt", row.names = FALSE, quote = FALSE)
write.table(meanstdName, "./result/feature_discription.txt", row.names = FALSE, quote = FALSE)





