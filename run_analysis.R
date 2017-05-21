library(plyr)

x_train <- read.table("UCI HAR Dataset/train/X_train.txt",header = F)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt",header = F)
sub_train <- read.table("UCI HAR Dataset/train/subject_train.txt",header = F)

x_test <- read.table("UCI HAR Dataset/test/X_test.txt",header = F)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt",header = F)
sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt",header = F)

features <- read.table("UCI HAR Dataset/features.txt")
features_names <- features[,2]

activities_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = F)


# 1. Merges the training and the test sets to create one data set.

merged_train <- cbind(y_train, sub_train, x_train )
merged_test <- cbind(y_test, sub_test, x_test )

tidy_data <- rbind(merged_train, merged_test)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
selected_features <- sapply(features_names, function(name){
  grepl("(mean|std)\\(\\)", name)
})

#keep Activity and Subject
features_names <- features_names[selected_features]
selected_features <- append(selected_features, c(TRUE, TRUE), 0)
tidy_data <- tidy_data[,selected_features]

# 3. Uses descriptive activity names to name the activities in the data set
tidy_data[,1] <- factor(tidy_data[,1], activities_labels[,1], activities_labels[,2])


# 4. Appropriately labels the data set with descriptive variable names.
features_names <- sapply(features_names,function(s) {
  readable<- gsub("\\(\\)", "", s)
  toupper(gsub("-", "_", readable))
})
colnames(tidy_data) <- append(features_names, c("ACTIVITY", "SUBJECT"), 0)


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mean_values <- ddply(tidy_data, .(ACTIVITY, SUBJECT), colwise(mean))

write.table(mean_values,"averages.txt",  row.name=FALSE)

str(mean_values)
