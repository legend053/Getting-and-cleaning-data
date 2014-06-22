library(reshape2)
#read data train
subject_train<-read.table("./data/UCI_HAR_Dataset/train/subject_train.txt",header=FALSE,stringsAsFactors=FALSE)
X_train<-read.table("./data/UCI_HAR_Dataset/train/X_train.txt",sep="",header=FALSE,stringsAsFactors=FALSE)
Y_train<-read.table("./data/UCI_HAR_Dataset/train/y_train.txt",header=FALSE,stringsAsFactors=FALSE)
features<-read.table("./data/UCI_HAR_Dataset/features.txt",header=FALSE,stringsAsFactors=FALSE)[,2]
activities<-read.table("./data/UCI_HAR_Dataset/activity_labels.txt",header=FALSE,stringsAsFactors=FALSE)

names(subject_train)<-"subjects"
names(X_train)<-features
names(Y_train)<-"activity_labels"

subject_train$ID<-c(1:dim(subject_train)[1])
X_train$ID<-c(1:dim(X_train)[1])
Y_train$ID<-c(1:dim(Y_train)[1])
#Merge the train data sets
subject_train<-subject_train[,c(dim(subject_train)[2],dim(subject_train)[2]-1)]	#move the order of columns
X_train<-X_train[,c(dim(X_train)[2],1:(dim(X_train)[2]-1))]
Y_train<-Y_train[,c(dim(Y_train)[2],dim(Y_train)[2]-1)]

train_mergedData<-merge(subject_train,X_train,by.x="ID",by.y="ID",all=TRUE)
train_mergedData<-merge(Y_train,train_mergedData,by.x="ID",by.y="ID",all=TRUE)
#Extracts only the measurements on the mean and standard deviation for each measurement
extract_features <- grepl("mean|std", features)
extract_features <- as.vector(features[extract_features])
extract_features <- append(extract_features,c("ID","subjects","activity_labels"))

length_features<-length(extract_features)
train_extract<-train_mergedData[,extract_features]
train_extract<-train_extract[,c((length_features-2):length_features,1:(length_features-3))] #obtain the extracted data sets

# begin process test data
subject_test<-read.table("./data/UCI_HAR_Dataset/test/subject_test.txt",header=FALSE,stringsAsFactors=FALSE)
X_test<-read.table("./data/UCI_HAR_Dataset/test/X_test.txt",sep="",header=FALSE,stringsAsFactors=FALSE)
Y_test<-read.table("./data/UCI_HAR_Dataset/test/y_test.txt",header=FALSE,stringsAsFactors=FALSE)

names(subject_test)<-"subjects"
names(X_test)<-features
names(Y_test)<-"activity_labels"

subject_test$ID<-c(1:dim(subject_test)[1])
X_test$ID<-c(1:dim(X_test)[1])
Y_test$ID<-c(1:dim(Y_test)[1])

subject_test<-subject_test[,c(dim(subject_test)[2],dim(subject_test)[2]-1)]	#move the order of columns
X_test<-X_test[,c(dim(X_test)[2],1:(dim(X_test)[2]-1))]
Y_test<-Y_test[,c(dim(Y_test)[2],dim(Y_test)[2]-1)]

test_mergedData<-merge(subject_test,X_test,by.x="ID",by.y="ID",all=TRUE)
test_mergedData<-merge(Y_test,test_mergedData,by.x="ID",by.y="ID",all=TRUE)

test_extract<-test_mergedData[,extract_features]
test_extract<-test_extract[,c((length_features-2):length_features,1:(length_features-3))]

# Merges the training and the test sets to create one data set.
total=rbind(test_extract,train_extract)
#Uses descriptive activity names to name the activities in the data set
for(id in 1:nrow(activities)){	#find the number and replace the corresponding description
total$activity_labels[total$activity_labels %in% activities$V1[id]]<-activities$V2[id]
}
#Appropriately labels the data set with descriptive variable names
changed_names<-names(total)
descriptivenames<-function(changed_names){
changed_names<-gsub("tBody", "Time_Body",changed_names)
changed_names<-gsub("tGravity", "Time_Gravity", changed_names)

changed_names<-gsub("fBody", "Fast_Fourier_Transform",changed_names)
changed_names<-gsub("fGravity", "Fast_Fourier_Transform", changed_names)

changed_names<-gsub("Acc", "_Accelerometer",changed_names)
changed_names<-gsub("Gyro", "_Gyroscope",changed_names)
return(changed_names)
}
names(total)<-descriptivenames(changed_names)

newname<-names(total)[-c(1:3)] #remove ID, subjects, activity_labels
totalDataMelt<-melt(total,id=c("subjects","activity_labels"),measure.vars=newname)
totalDataMean<-dcast(totalDataMelt, subjects+activity_labels ~ variable, mean) #If focus on activities, #trainDataMean<-dcast(trainDataMelt, activity_labels+subjects ~ variable, mean)

#write files
write.csv(total,file="total.txt",row.names=FALSE) #first raw tidy data
write.csv(totalDataMean,file="totalDataMean.txt",row.names=FALSE)
