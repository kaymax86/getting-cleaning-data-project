## Getting and Cleaning Data Course Project
## Kayla Maxwell
## 03-11-2016

##Steps to download online zip folder
if(!file.exists("./data"))
{dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, "./data/get_data.zip")
setwd("./data")
unzip("get_data.zip")
list.files()

##read in the training and test files
features     = read.table('UCI HAR Dataset/features.txt',header=FALSE)
activityType = read.table('UCI HAR Dataset/activity_labels.txt',header=FALSE)
subjectTrain = read.table('UCI HAR Dataset/train/subject_train.txt',header=FALSE)
xTrain       = read.table('UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain       = read.table('UCI HAR Dataset/train/y_train.txt',header=FALSE)
subjectTest = read.table('UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest       = read.table('UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest       = read.table('UCI HAR Dataset/test/y_test.txt',header=FALSE)

##assign descriptive column names
colnames(activityType)=c('activityID','activityType')
colnames(subjectTrain)="subjectID"
colnames(subjectTest)="subjectID"
colnames(xTrain)=features[,2]
colnames(xTest)=features[,2]
colnames(yTrain)="activityID"
colnames(yTest)="activityID"

#merge data sets
Train=cbind(yTrain, subjectTrain, xTrain)
Test=cbind(yTest, subjectTest, xTest)
TrainTest = rbind(Train, Test)

#extract only mean and sd for each measurement
TTcolnames <- colnames(TrainTest)
mean_sd = (grepl("activity..",TTcolnames) | grepl("subject..",TTcolnames) | 
             grepl("-mean..",TTcolnames) & !grepl("-meanFreq..",TTcolnames) 
           & !grepl("mean..-",TTcolnames) | grepl("-std..",TTcolnames) 
           & !grepl("-std()..-",TTcolnames))
FinalData=TrainTest[mean_sd==TRUE]

##merge final data with activity table to get descriptive activity names
FinalData = merge(FinalData,activityType,by='activityID',all.x=TRUE)
TTcolnames <- colnames(FinalData)

##Tidy data
FinalData$activity <- factor(FinalData$activityID, levels = activityType[,1], labels = activityType[,2])
FinalData$subject <- as.factor(FinalData$subjectID)
FinalData.melted <- melt(FinalData, id = c("subjectID", "activityID"))
names(FinalData) <- gsub('\\(|\\)',"",names(FinalData), perl = TRUE) ##remove parentheses
names(FinalData) <- make.names(names(FinalData))
names(FinalData) <- gsub('Acc',"Acceleration",names(FinalData))
names(FinalData) <- gsub('GyroJerk',"AngularAcceleration",names(FinalData))
names(FinalData) <- gsub('Gyro',"AngularSpeed",names(FinalData))
names(FinalData) <- gsub('Mag',"Magnitude",names(FinalData))
names(FinalData) <- gsub('^t',"TimeDomain.",names(FinalData))
names(FinalData) <- gsub('^f',"FrequencyDomain.",names(FinalData))
names(FinalData) <- gsub('\\.mean',".Mean",names(FinalData))
names(FinalData) <- gsub('\\.std',".StandardDeviation",names(FinalData))
names(FinalData) <- gsub('Freq\\.',"Frequency.",names(FinalData))
names(FinalData) <- gsub('Freq$',"Frequency",names(FinalData))
FinalTidyData = ddply(FinalData, c("subjectID","activityID"), numcolwise(mean))
write.table(FinalTidyData, file = "FinalTidyData.txt",row.names = FALSE)

