library(dplyr)

filename <- "DS3_project.zip"

# Checking if archive already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename)
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

list.files("./UCI HAR Dataset", recursive = TRUE)

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activityId", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activityId")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activityId")


## Step 1
# Merges the training and the test sets to create one data set
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged <- cbind(Subject, Y, X)

## Step 2
# Extracts only the measurements on mean and standard deviation
# for each measurement
mean_std <- Merged %>% select(subject, activityId, contains("mean"), contains("std"))


## Step 3
# Uses descriptive activity names to name the activities in the 
# data set
mean_std$activityId <- activities[mean_std$activityId, 2]


## Step 4
# Appropriately labels the data set with descriptive variable
# names
names(mean_std)[2] = "activity"
names(mean_std)<-gsub("Acc", "Accelerometer", names(mean_std))
names(mean_std)<-gsub("Gyro", "Gyroscope", names(mean_std))
names(mean_std)<-gsub("BodyBody", "Body", names(mean_std))
names(mean_std)<-gsub("Mag", "Magnitude", names(mean_std))
names(mean_std)<-gsub("^t", "Time", names(mean_std))
names(mean_std)<-gsub("^f", "Frequency", names(mean_std))
names(mean_std)<-gsub("tBody", "TimeBody", names(mean_std))
names(mean_std)<-gsub("-mean()", "Mean", names(mean_std), ignore.case = TRUE)
names(mean_std)<-gsub("-std()", "STD", names(mean_std), ignore.case = TRUE)
names(mean_std)<-gsub("-freq()", "Frequency", names(mean_std), ignore.case = TRUE)
names(mean_std)<-gsub("angle", "Angle", names(mean_std))
names(mean_std)<-gsub("gravity", "Gravity", names(mean_std))


## Step 5
# From the data set step 4, creates a second, independent tidy
# data set with the average of each variable for each activity
# and each subject.
TidyData <- mean_std %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean))
write.table(TidyData, "tidy-data.txt", row.name=FALSE)