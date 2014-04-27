test <- read.table("./UCI HAR Dataset/test/X_test.txt")
head(test)
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
head(train)

# Uses descriptive activity names to name the activities in the data set
features <- read.table("features.txt")

colnames(test) <- features[,2]
colnames(train) <- features[,2]



# merges the training and the test sets 
data <- rbind(test,train)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
mean <- data[,grep("mean()", names(data), fixed=T)]
std <- data[,grep("std()", names(data), fixed=T)]

extra <- cbind(mean,std)




# Appropriately labels the data set with descriptive activity names. 
a_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
a_labels[,2] <- as.character(a_labels[,2])

y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

for (i in 1:6) {
        y_test[y_test==i]=a_labels[i,2]
}
for (i in 1:6) {
        y_train[y_train==i]=a_labels[i,2]
}

activity <- rbind(y_test,y_train)
colnames(activity) <- "activity"
extra$activity <- activity[,1]
data$activity <- activity[,1]



# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

subject <- rbind(subject_test, subject_train)
colnames(subject) <- "subject"
extra$subject <- subject[,1]
data$subject <- subject[,1]


aggregate(extra[,1]~y[,1] * subject[,1] , data=extra, mean())

extra$n=paste(extra$activity, extra$subject, sep="_")
data$n=paste(data$activity, data$subject, sep="_")

tidy_data0 <- aggregate(data[,1:561], by=list(data$n), mean)
tidy_data <- aggregate(extra[,1:66], by=list(extra$n), mean)

write.table(tidy_data, file="tidy_data.txt", sep="\t")
#write.table(tidy_data0, file="tidy_data0.txt", sep="\t")
