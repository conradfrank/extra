library(dplyr)
library(data.table)
library(tidyr)

# unzip file to data folder

if(!file.exists("./data")){dir.create("./data")}
if(!file.exists("./data/UCI HAR Dataset") | !file.exists("./data/ProjectData.zip")){
  url  <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  
  download.file(url , destfile = "./data/ProjectData.zip", mode = "wb", method = "libcurl")
}


# unzip the file
if(!file.exists("UCI HAR Dataset")){
unzip(zipfile="./data/ProjectData.zip",exdir="./data")
}


# to check the unzipped folder in Data folder and get the list of files
list.files("./data")
path_data <- file.path("./data", "UCI HAR Dataset")
files <- list.files(path_data, recursive = TRUE)
files

# read test data
Xtest <- read.table(file.path(path_data, "test", "X_test.txt"), header = FALSE)
Ytest <- read.table(file.path(path_data, "test", "y_test.txt"), header = FALSE)
Subjecttest <- read.table(file.path(path_data, "test", "subject_test.txt"), header = FALSE)

# read train data
Xtrain <- read.table(file.path(path_data, "train", "X_train.txt"), header = FALSE)
Ytrain <- read.table(file.path(path_data, "train", "Y_train.txt"), header = FALSE)
subjecttrain <- read.table(file.path(path_data, "train", "subject_train.txt"), header = FALSE)

# read feature labels data
feature <- read.table(file.path(path_data, "features.txt"), header = FALSE)


#Merging the training and the test data sets to create one data set
finaldata_subject <- rbind(Subjecttest, subjecttrain)
finaldata_feature <- rbind(Xtest, Xtrain)
finaldata_activity <- rbind(Ytest, Ytrain)
names(finaldata_subject) <- c("subject")
names(finaldata_activity) <- c("activity")
names(finaldata_feature) <- feature$V2
merged_data <- cbind(finaldata_activity, finaldata_feature, finaldata_subject)

#Extracting only the measurements on the mean and standard 
#deviation for each measurement
col_names <- colnames(merged_data)
mean_std_labels <- col_names[grep(".*mean.*|.*std.*", col_names, ignore.case = TRUE)]
mean_std_labels <- c(as.character(mean_std_labels), "subject", "activity")
mean_std_data <- subset(merged_data, select = mean_std_labels)

#Uses descriptive activity names to name the activities in the data set
activitylabel <- read.table(file.path(path_data, "activity_labels.txt"), header = FALSE)
mean_std_data$activity <- factor(mean_std_data$activity, levels = activitylabel[,1], labels = activitylabel[,2])

#Appropriately labels the data set with descriptive variable names
names(mean_std_data)<-gsub("Acc", "Accelerometer", names(mean_std_data))
names(mean_std_data)<-gsub("Gyro", "Gyroscope", names(mean_std_data))
names(mean_std_data)<-gsub("BodyBody", "Body", names(mean_std_data))
names(mean_std_data)<-gsub("Mag", "Magnitude", names(mean_std_data))
names(mean_std_data)<-gsub("^t", "Time", names(mean_std_data))
names(mean_std_data)<-gsub("^f", "Frequency", names(mean_std_data))
names(mean_std_data)<-gsub("tBody", "TimeBody", names(mean_std_data))
names(mean_std_data)<-gsub("-mean()", "Mean", names(mean_std_data), ignore.case = TRUE)
names(mean_std_data)<-gsub("-std()", "STD", names(mean_std_data), ignore.case = TRUE)
names(mean_std_data)<-gsub("-freq()", "Frequency", names(mean_std_data), ignore.case = TRUE)
names(mean_std_data)<-gsub("angle", "Angle", names(mean_std_data))
names(mean_std_data)<-gsub("gravity", "Gravity", names(mean_std_data))

##From the data set in step 4, creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject
mean_std_data$subject <- as.factor(mean_std_data$subject)
tidydata <- aggregate(. ~ subject + activity, mean_std_data, FUN = "mean")
tidydata <- tidydata[order(tidydata$subject,tidydata$activity),]
write.table(tidydata, file = "Tidy.txt", row.names = FALSE)

