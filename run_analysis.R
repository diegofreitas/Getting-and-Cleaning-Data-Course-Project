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

# 4. Appropriately labels the data set with descriptive variable names.
colnames(x_train) <- features_names
colnames(x_test) <- features_names

colnames(y_train) <- c("ACTIVITY")
colnames(y_test) <- c("ACTIVITY")

colnames(sub_train) <- c("SUBJECT")
colnames(sub_test) <- c("SUBJECT")

# 1. Merges the training and the test sets to create one data set.

merged_train <- cbind(y_train, sub_train, x_train )
merged_test <- cbind(y_test, sub_test, x_test )

tidy_data <- rbind(merged_train, merged_test)

# 3. Uses descriptive activity names to name the activities in the data set
tidy_data$ACTIVITY <- factor(tidy_data$ACTIVITY, activities_labels[,1], activities_labels[,2])

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
measurement_mean <- colwise(mean)(tidy_data[,3:563])
measurement_sd <- colwise(sd)(tidy_data[,3:563])

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
summary_df <- ddply(tidy_data, .(ACTIVITY, SUBJECT), colwise(mean))

write.table(summary_df,"averages.txt",  row.name=FALSE)

