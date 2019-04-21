#run_analysis.R takes the data downloaded from the UCI HAR dataset
#and performs the data preparation followed by the 5 steps as defined
#in the assignment 1. Merge trraining and test 2. Extract mean and 
#standard deviation 3. Use descriptive names 4. Appropiately label datsets
#5. Create independent "tidy" dataset

#Download the data and setup data frames
#Check to see if file exists and is unzipped, if not downoad and unzip
filename<-"Coursera_DS3_Final.zip"
if(!file.exists(filename)){
    fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL,filename,metho="curl")
}
if(!file.exists("UCI HAR Dataset")){
    unzip(filename)
}

#Read data and assign data frames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Begin analysis
#1. Merge training and test data
XX<-rbind(x_train,x_test)
YY<-rbind(y_train,y_test)
Subject<-rbind(subject_train,subject_test)
Merged<-cbind(Subject,YY,XX)

#2. Extract mean and standard deviation
extracted_data<-Merged %>% select(subject, code, contains("mean"), contains("std"))

#3. Use descriptive names
extracted_data$code<-activities[extracted_data$code,2]

#4. Appropriately label datasets
names(extracted_data)[2] = "activity"
names(extracted_data)<-gsub("Acc", "Accelerometer", names(extracted_data))
names(extracted_data)<-gsub("Gyro", "Gyroscope", names(extracted_data))
names(extracted_data)<-gsub("BodyBody", "Body", names(extracted_data))
names(extracted_data)<-gsub("Mag", "Magnitude", names(extracted_data))
names(extracted_data)<-gsub("^t", "Time", names(extracted_data))
names(extracted_data)<-gsub("^f", "Frequency", names(extracted_data))
names(extracted_data)<-gsub("tBody", "TimeBody", names(extracted_data))
names(extracted_data)<-gsub("-mean()", "Mean", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-std()", "STD", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("-freq()", "Frequency", names(extracted_data), ignore.case = TRUE)
names(extracted_data)<-gsub("angle", "Angle", names(extracted_data))
names(extracted_data)<-gsub("gravity", "Gravity", names(extracted_data))

#5. Create independent "tidy" dataset
TidyData <- extracted_data %>%
    group_by(subject, activity) %>%
    summarise_all(list(mean))
write.table(TidyData, "TidyData.txt", row.name=FALSE)