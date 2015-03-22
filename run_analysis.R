library(dplyr)

if(!file.exists("data")){
  dir.create("data")
}

if(!file.exists("./data/Dataset.zip")){
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/Dataset.zip", mode = "wb")
}

unzip("./data/Dataset.zip", exdir=".")

## read in the X_test.txt file and X_train.txt

testData<-read.table("./UCI HAR Dataset/test/X_test.txt", sep='',header=FALSE)
trainData<-read.table("./UCI HAR Dataset/train/X_train.txt", sep='',header=FALSE)

## training and test sets to make one data set

## Add Column names to dataframe

## load the Column Names from features

featuresData<-read.table("./UCI HAR Dataset/features.txt", sep='',header=FALSE)
testDataNamed<-testData
trainDataNamed<-trainData
colnames(testDataNamed)<-featuresData$V2
colnames(trainDataNamed)<-featuresData$V2

## get a vector of subject_testData and y_test for  activity

subject_testData<-read.table("./UCI HAR Dataset/test/subject_test.txt", sep='',header=FALSE)
y_testData<-read.table("./UCI HAR Dataset/test/y_test.txt", sep='',header=FALSE)

subject_trainData<-read.table("./UCI HAR Dataset/train/subject_train.txt", sep='',header=FALSE)
y_trainData<-read.table("./UCI HAR Dataset/train/y_train.txt", sep='',header=FALSE)


## add the vectors as columns to the testDataNamed dataframe
testDataNamed$subject<-subject_testData$V1
trainDataNamed$subject<-subject_trainData$V1


testDataNamed$y<-y_testData$V1
trainDataNamed$y<-y_trainData$V1

## get the activity_labels

activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt", sep='',header=FALSE)

## merge the data of the activity_labels and the testDataNamed dataframe

mergeTestData = merge(testDataNamed,activity_labels,by.x="y",by.y="V1",all=TRUE)
mergeTrainData = merge(trainDataNamed,activity_labels,by.x="y",by.y="V1",all=TRUE)

## rename V2 of the mergeTestData

names(mergeTestData)[names(mergeTestData)=="V2"]<-"activity"
names(mergeTrainData)[names(mergeTrainData)=="V2"]<-"activity"

##library(plyr) package did not seem to work
## http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/
## rename(mergeTestData,c("V2"="activity"))

##rbind the mergeTestData and mergeTrainData to create a mergeData set

mergeData<-rbind(mergeTestData, mergeTrainData)

## extract data on only the mean and standard deviation for each measurement
## whatever this means. So I am extracting columns where the names() function
## returns true for grep(mean|std, names(df))
## however, I wanted to ommit meanFreq() as it wasn't the mean of the measurement
## but the mean Frequency of the measurement (whatever that is)

meanStdVec<-grepl("mean\\(\\)|std\\(\\)",names(mergeData))

## subset mergeData based on the meanStdVec

submergeData<-mergeData[,meanStdVec]
## add back in the activity and the subject columns
submergeData$activity<-mergeData$activity
submergeData$subject<-mergeData$subject

#convert subject from integer to factor and call the dataset FinalData

submergeData$subjectFactor<-factor(submergeData$subject)
FinalData<-submergeData[,!names(submergeData)=="subject"]

FinalData<-rename(FinalData, subject=subjectFactor)

##Find the mean group by activity and group by subject

aggData<-aggregate(FinalData,by=list(FinalData$activity,FinalData$subject),FUN=mean, na.rm=TRUE)

## create TidyData

TidyData<-select(aggData, -(activity:subject))
TidyData<-rename(TidyData,activity = Group.1, subject = Group.2)

