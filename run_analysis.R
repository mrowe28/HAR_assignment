##This script extracts information from several files of the "Human Activity
##Recognition Using Smartphones" dataset and combines it into a single data
##frame.  Labels are assigned for each variable and a subset of the data
##corresponding to the mean and standard deviation of each measurement is
##extracted.  The data are sorted and aggregated by calculating the mean for
##subsets of each measurement variable, grouped by subject and activity.  The
##resulting output is a .txt file containing a tidy data set.

##The following portion of the script reads in the subject and activity
##identification numbers and corresponding measurements for the test set and 
##combines them into a single data frame.
test_subjects <- read.table("subject_test.txt")
test_activities <- read.table("y_test.txt")
test_measurements <- read.table("X_test.txt")
test_complete <- cbind(test_subjects,test_activities,test_measurements)

##The following portion of the script assigns labels for the subject, activity,
##and measurement variables.  Part of the process involves reformating and
##cleaning up the labels for the measurements, which are derived from the
##"features.txt" file.
first_column_labels <- c("Subject","Activity")
features <- read.table("features.txt")
measurement_names <- as.vector(features[,2],mode = "character")
measurement_names <- sub("-",".",measurement_names,fixed=TRUE)
measurement_names <- sub("-",".",measurement_names,fixed=TRUE)
measurement_names <- sub("(","",measurement_names,fixed=TRUE)
measurement_names <- sub(")","",measurement_names,fixed=TRUE)
measurement_names <- sub("BodyBody","Body",measurement_names,fixed=TRUE)
column_labels <- c(first_column_labels, measurement_names)
colnames(test_complete) <- column_labels

##The following portion of the script repeats the process of reading in and
##combining the data for the training set.  Labels for each variable are
##applied in the same manner as above.
train_subjects <- read.table("subject_train.txt")
train_activities <- read.table("y_train.txt")
train_measurements <- read.table("X_train.txt")
train_complete <- cbind(train_subjects,train_activities,train_measurements)
colnames(train_complete) <- column_labels

##The following portion of the script combines the data frames for the test
##and training sets and extracts data corresponding to the mean and
##standard deviation for each measurement.
data_complete <- rbind(test_complete,train_complete)
features <- read.table("features.txt")
filter_vector <- c(1,2)
for(i in 1:length(features[,1])){
        if(grepl("mean()",features[i,2],fixed=TRUE)==TRUE |
                   grepl("std()",features[i,2],fixed=TRUE)==TRUE){
                filter_vector[i+2] <- i+2
        }
}
filter_vector <- filter_vector[!is.na(filter_vector)]
data_filter <- data_complete[,filter_vector]

##The following portion of the script sorts the data by the subject and
##activity variables and replaces the activity identification numbers with
##descriptive variable names.
sorted_data <- data_filter[with(data_filter,order(Subject,Activity)),]
sorted_data$Activity <- as.character(sorted_data$Activity)
activity_names <- read.table("activity_labels.txt",colClasses=c("integer","character"))
for(j in 1:length(activity_names[,2])){
        sorted_data$Activity[sorted_data$Activity == as.character(j)] <- activity_names[j,2]
}
row.names(sorted_data) <- c(1:length(sorted_data[,1]))

##The following portion of the script aggregates the data by calculating the
##mean for subsets of each measurement variable, grouped by subject and
##activity.  Lastly, the script outputs a .txt file containing the tidy data.
library("dplyr")
grouped_data <- group_by(sorted_data,Subject,Activity)
aggregated_data <- summarise_each(grouped_data,funs(mean))
write.table(aggregated_data,file ="tidy_HAR_dataset.txt",quote=FALSE,row.names=FALSE)