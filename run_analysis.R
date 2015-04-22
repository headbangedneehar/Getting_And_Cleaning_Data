library(plyr)
library(dplyr)
library(qdap)
##--------------------STEP 1----------------------------------
##Merges the training and the test sets to create one data set.
features<-read.csv("features.txt",header=F,sep="",stringsAsFactor=FALSE)
Xtest<-read.csv("test/X_test.txt",sep="",header=F)
subtest<-read.csv("test/subject_test.txt",header=F)
Xtest<-cbind(ID=1:length(Xtest[,1]),Subject=subtest,Data="TEST",Xtest)
names(Xtest)[1:3]<-c("ID","Subject","Data")

Xtrain<-read.csv("train/X_train.txt",sep="",header=F)
subtrain<-read.csv("train/subject_train.txt",header=F)
Xtrain<-cbind(ID=1:length(Xtrain[,1]),Subject=c(subtrain),Data="TRAIN",Xtrain)
names(Xtrain)[1:3]<-c("ID","Subject","Data")
##Obtain merged data of test and train data sets
mergeData<-merge(Xtest,Xtrain,all=TRUE)
names(mergeData)<-c("ID","Subject","Data",features[,2])
library(dplyr)
mergeData<-arrange(mergeData,Data,ID)
##------------------------------------------------------------

##--------------------STEP 2----------------------------------
##Extracts only the measurements on the mean and standard deviation for each measurement.
text<-names(mergeData)
reqCols<-grep(pattern ="(mean|std)", text,value = T, ignore.case = F)
reqCols<-grep(pattern ="(meanFreq)", reqCols,value = T,invert = T, ignore.case = F)
mergeData<-mergeData[,c("ID","Subject","Data",reqCols)]
##------------------------------------------------------------

##--------------------STEP 3----------------------------------
##Uses descriptive activity names to name the activities in the data set
ytest<-read.csv("test/y_test.txt",header=F)
ytrain<-read.csv("train/y_train.txt",header=F)
testlabels<-c(ytest[,1],ytrain[,1])
activities<-factor(testlabels,labels=c("WALKING","WALKING_UPSTAIRS",
                                       "WALKING_DOWNSTAIRS","SITTING",
                                       "STANDING","LAYING"))
mergeData$Activity<-activities
mergeData<-mergeData[,c(1:3,length(mergeData[1,]),4:(length(mergeData[1,])-1))]
##------------------------------------------------------------

##--------------------STEP 4----------------------------------
##Appropriately labels the data set with descriptive variable names. 
text<-c("tBody","Acc","-mean()","-std()","Jerk","Gyro","fBody",
        "Mag","tGravity","fBodyBody","-X","-Y","-Z")
replacement<-c("Time_Body","_Acc","_Mean","_Std","_Jerk","_Gyro",
               "FFT_Body","_Mag","Time_Gravity","FFT_Body","(X)","(Y)","(Z)")
names(mergeData)<-mgsub(text ,replacement,ignore.case = F,names(mergeData))
##------------------------------------------------------------

##--------------------STEP 5----------------------------------
##From the data set in step 4, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject.
mergeData<-group_by(mergeData,Subject,Activity)
summarise_each(mergeData,funs(mean))->tidyData
tidyData<-data.frame(tidyData)
tidyData$Data<-NULL
tidyData$ID<-NULL
rwidth<-NULL
rwidth[1:2]=12
rwidth[3:length(names(tidyData))]=25
tidyData<-mutate(Activity=as.character(Activity),tidyData)
tidyData<-rbind(names(tidyData),tidyData)
format(tidyData,width=rwidth,justify="left",quote=F,col.names=F)->tidyData
names(tidyData)<-NULL
write.table(tidyData,"Tidy_Data.txt",quote = F,row.name=F)

##-------------------------------------------------------
##TEXT FILE OBTAINED-------------------------------------