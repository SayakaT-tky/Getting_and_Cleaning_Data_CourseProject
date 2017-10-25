options(width=80)
library(dplyr)
library(RCurl)

setwd("C:/Users/Sayaka Tanaka/Documents/2017à»ç~/Self-learning/Coursera/DataScience_byJHUniv/C3_CleaningData/CourseProject")

##download file
filesource<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile<-"./C3projectdata.zip"
download.file(filesource,zipfile)

##unzip file
unzip(zipfile)

##load data sets to R
list.files()
list.files("./UCI HAR Dataset/test")
list.files("./UCI HAR Dataset/train")
fts<-read.table("./UCI HAR Dataset/features.txt")
activitylbl<-read.table("./UCI HAR Dataset/activity_labels.txt")
x_test<-read.table("./UCI HAR Dataset/test/x_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
x_train<-read.table("./UCI HAR Dataset/train/x_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
dim(x_test)
dim(y_test)
dim(subject_test)
dim(x_train)
dim(y_train)
dim(subject_train)
dim(fts)

##====
##Step_1: Merge the training and the test sets to create one data set.
##====

#merge the train data, train activity labels, and train subject IDs
mergetrain<-cbind(x_train,y_train,subject_train)
#merge the test data, test activity labels, and test subject IDs
mergetest<-cbind(x_test,y_test,subject_test)

#merge training and test data sets
Dbase<-rbind(mergetest,mergetrain)
names(Dbase)<-c(as.character(fts[,2]),"Activity","Subject")

##====
##Step_2:Extract only the measurements on the mean and standard deviation 
##for each measurement.
##====
colselector<-grepl(".*mean\\(\\)|.*std\\(\\)|Activity|Subject",colnames(Dbase))
Dsub<-Dbase[,colselector]

##====
##Step_3:Use descriptive activity names to name the activities in the data set
##====
Dsub$Activity<-factor(Dsub$Activity,levels=activitylbl[,1],labels=activitylbl[,2])

##====
##Step_4:Appropriately label the data set with descriptive variable names.
##====
Colcorrection<-names(Dsub)

#Correct possible typo
Colcorrection<-gsub("BodyBody","Body",Colcorrection)
#Convert labels to descriptive names
Colcorrection<-gsub("^f","FreqencyDomain",Colcorrection)
Colcorrection<-gsub("^t","TimeDomain",Colcorrection)
Colcorrection<-gsub("Mag","Magnitude",Colcorrection)
Colcorrection<-gsub("-mean","Mean",Colcorrection)
Colcorrection<-gsub("-std","Std",Colcorrection)
#Delete special letters
Colcorrection<-gsub("\\(\\)","",Colcorrection)
Colcorrection<-gsub("\\-","",Colcorrection)

#Align the order of abbreviations with those of the Magnitude variables (to enable simple discription in the codebook)
Colcorrection<-gsub("MeanX","XMean",Colcorrection)
Colcorrection<-gsub("MeanY","YMean",Colcorrection)
Colcorrection<-gsub("MeanZ","ZMean",Colcorrection)
Colcorrection<-gsub("StdX","XStd",Colcorrection)
Colcorrection<-gsub("StdY","YStd",Colcorrection)
Colcorrection<-gsub("StdZ","ZStd",Colcorrection)

names(Dsub)<-Colcorrection

##====
##Step_5:Create a second, independent tidy data set with the average of each variable 
##for each activity and each subject.
##====
Dtidy <- Dsub %>% group_by(Activity,Subject) %>% summarize_each(funs(mean))

##====
##Step_6: Output the result to "tidydata.txt"
##====
write.table(Dtidy,"tidydata.txt",row.names = FALSE, quote=FALSE)
