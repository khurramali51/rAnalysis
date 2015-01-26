run_Analysis <- function(){
#Assumptions:
#1. All four files are situated in "data" folder in working directory
#2. txt files X_train.txt,y_train.txt and subject_train.txt are in "data/train" folder of working directory
#3. txt files X_test.txt,y_test.txt and subject_test.txt are in "data/test" folder of working directory


#Req 1. Merges the training and test sets to create one data set.

#Apporach: creating two data set one for train and test using cbind and then used rbind to 
#          to combine both the data set.  


#Step 1.  using read.table to read the data and cache it 

temp_x_train<-read.table("./data/train/X_train.txt")
temp_y_train<-read.table("./data/train/y_train.txt")
temp_x_test<-read.table("./data/test/X_test.txt")
temp_y_test<-read.table("./data/test/y_test.txt")
temp_subject_test<-read.table("./data/test/subject_test.txt")
temp_subject_train<-read.table("./data/train/subject_train.txt")



#Step2: 
#get feature names into Feature_CName
Feature_CName<-read.table("./data/features.txt")

#Step3:
#associate the feature name with test and train data set X_files
names(temp_x_train)<-Feature_CName$V2
names(temp_x_test)<-Feature_CName$V2


#Step4: 
#associate label files with name 

names(temp_y_train)<-"activity"
names(temp_y_test)<-"activity"


#step5: 
#associate the subject file with names
names(temp_subject_train)<-"Subject"
names(temp_subject_test)<-"Subject"



#Step6:
#create complete test data set
#create complete train data set

test_merge<-cbind(temp_subject_test,temp_y_test,temp_x_test)
train_merge<-cbind(temp_subject_train,temp_y_train,temp_x_train)
Merge_Data<-rbind(train_merge,test_merge)




#Req 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#Step1. Extract the mean and std. dev column names 
ColName <- grep("(mean|std)\\(\\)", names(Merge_Data))


#Step2. Cache the data 
SubSet_Merge_Data <- Merge_Data[, ColName]



#Req 3. Uses descriptive activity names to name the activities in the data set

#Step1. creating Activity names
ActivityNames <-c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
 

#Step2. Associate ActivityNames with Merge_Data$activity
Merge_Act_Data<-ActivityNames[Merge_Data$activity]



#Req 4. Appropriately labels the data set with descriptive variable names. 

#Step1. load the library(stringr)
library(stringr)

#Step2. 
#replacing extra . at the end of column names 
#replacing mean with Mean
#replacing tBody with Time.Body
#replacing tGravity with Time.Gravity
#replacing f with Frequency
#replacing BodyBody with Body

colnames(Merge_Data) <- str_replace_all(colnames(Merge_Data), "[\\.]+$", "")
colnames(Merge_Data) <- str_replace_all(colnames(Merge_Data),"(-mean)","-MEAN-")
colnames(Merge_Data) <- str_replace_all(colnames(Merge_Data),"tBody","Time.Body")
colnames(Merge_Data) <- str_replace_all(colnames(Merge_Data),"tGravity","Time.Gravity")
colnames(Merge_Data) <- str_replace_all(colnames(Merge_Data),"^f","Frequency.")
colnames(Merge_Data) <- str_replace_all(colnames(Merge_Data),"BodyBody","Body")



#Req 5. From the data set in step 4, creates a second, 
#independent tidy data set 
#with the average of each variable for 
#each activity and each subject.	


#Step1. load library (reshape2)
library(reshape2)

#Step2. melt data (taking wide format data in long format for tidyness) 
temp_melt_Data <- melt(Merge_Data, id=c("Subject","activity"))

#Step3. using dcast
tidyData <- dcast(temp_melt_Data, Subject+activity ~ variable, mean)


#Step4.writing file
write.table(tidyData, "my_NewFile.txt", row.names=FALSE)

}


