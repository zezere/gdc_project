library("R.utils")

# Loading data

activity_labels_pathname <- "UCI HAR Dataset/activity_labels.txt"
activity_labels_pathname <- Arguments$getReadablePathname(activity_labels_pathname)
activity_labels <- read.table(activity_labels_pathname)

features_pathname <- "UCI HAR Dataset/features.txt"
features_pathname <- Arguments$getReadablePathname(features_pathname)
features <- read.table(features_pathname)

subject_test_pathname <- "UCI HAR Dataset/test/subject_test.txt"
subject_test_pathname <- Arguments$getReadablePathname(subject_test_pathname)
subject_test <- read.table(subject_test_pathname)
X_test_pathname <- "UCI HAR Dataset/test/X_test.txt"
X_test_pathname <- Arguments$getReadablePathname(X_test_pathname)
X_test <- read.table(X_test_pathname)
Y_test_pathname <- "UCI HAR Dataset/test/Y_test.txt"
Y_test_pathname <- Arguments$getReadablePathname(Y_test_pathname)
Y_test <- read.table(Y_test_pathname)

subject_train_pathname <- "UCI HAR Dataset/train/subject_train.txt"
subject_train_pathname <- Arguments$getReadablePathname(subject_train_pathname)
subject_train <- read.table(subject_train_pathname)
X_train_pathname <- "UCI HAR Dataset/train/X_train.txt"
X_train_pathname <- Arguments$getReadablePathname(X_train_pathname)
X_train <- read.table(X_train_pathname)
Y_train_pathname <- "UCI HAR Dataset/train/Y_train.txt"
Y_train_pathname <- Arguments$getReadablePathname(Y_train_pathname)
Y_train <- read.table(Y_train_pathname)

# Merging test and train datasets into one
df_test <- cbind(subject_test, Y_test, X_test)
df_train <- cbind(subject_train, Y_train, X_train)
df <- rbind(df_test, df_train)

# Labelling first two rows appropriately
names(df)[1] <- "subjectID"
names(df)[2] <- "activityID"

# Leaving only columns with measurements of mean and standard deviation
features$V2 <- as.character(features$V2)
mean_std_columns <- features[grep("mean|std", features$V2), ]
mean_std_column_names <- paste("V", as.character(mean_std_columns$V1), sep="")
df_mean_std <- df[, c("subjectID", "activityID", mean_std_column_names)]

# Labelling measurement columns appropriately
names(df_mean_std)[-1:-2] <- mean_std_columns$V2

# Replacing activity IDs with activity names
activity_labels$V1 <- as.character(activity_labels$V1)
activity_labels$V2 <- as.character(activity_labels$V2)
df_mean_std$activityID <- as.character(df_mean_std$activityID)
for (i in 1:nrow(activity_labels)) {
    df_mean_std[grep(activity_labels$V1[i], df_mean_std$activityID), "activityID"] <- activity_labels$V2[i]
}
names(df_mean_std)[2] <- "activityLabel"

# calculating means of all measurements for each pair of subject and activity
library(reshape2)
df_melted <- melt(data=df_mean_std, id=c("subjectID","activityLabel"))
df_means <- dcast(df_melted, subjectID + activityLabel ~ variable, mean)

# writes the result into file
write.table(df_means, file="tidy.txt", row.names=FALSE)


