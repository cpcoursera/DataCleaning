
#loading a few libraries that are needed to ease the whole process. loading plyt & dplyr is probably
#an overhead but as I was experimenting I used both and then I was a bit lazy to figure out if I have
#to remove one of the two :) 
library(plyr)
library(dplyr)
library(data.table)

#reading the training data, we need the feature vectors (x_train) the activities (y_train) that each one of the vectors correspond to
#and finaly the subjects that these observations belong to (subject_train).
X_train <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")

#Same thing for the testing data sets.
X_test <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")

#we are giving a column name to the activites and subjects to make things a bit cleaner to read
colnames(y_train) <- c("activity_id")
colnames(subject_train) <- c("subject_id")
colnames(y_test) <- c("activity_id")
colnames(subject_test) <- c("subject_id")

#we are also reading the features names of the dataset that will be used as the column names of the feature vectors, these
#are the same for both the test and the training data
features <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
l <- lapply(features$V2, as.character)

#we are also doing some cleaning to the feature names to make them more suitable to act as collumn names, especially
#the removal of some characters that make working with scripts difficult, e.g. "-" etc. Apart from that we'll use the 
#names as they are, they are already quite descriptive.
l <- sub("\\(\\)", "", l)
l <- sub("\\(", "_", l)
l <- sub("\\)", "_", l)
l <- sub(",", "_", l)
l <- sub("-mean-","_mean_",l)
l <- sub("-std-","_std_",l)

colnames(X_train) <- l
colnames(X_test) <- l

#we "merge" the feature vectors with the subject ids and the activities ids to have only one table with all the data
train <- cbind(subject_train, y_train, X_train)
test <- cbind(subject_test, y_test, X_test)

merged <- rbind(train, test)
#we use grep to keep only the columns that we are interesting at, anything that contains the term "mean" and "std" as
#together with the subject and activity ids
l <- grepl("mean|std|subject_id|activity_id", names(merged))
merged_filtered <- merged[l]

#we change the activity_ids from numerical values to self explenatory strings
merged_filtered$activity_id[merged_filtered$activity_id==1] <- "WALKING"
merged_filtered$activity_id[merged_filtered$activity_id==2] <- "WALKING_UPSTAIRS"
merged_filtered$activity_id[merged_filtered$activity_id==3] <- "WALKING_DOWNSTAIRS"
merged_filtered$activity_id[merged_filtered$activity_id==4] <- "SITTING"
merged_filtered$activity_id[merged_filtered$activity_id==5] <- "STANDING"
merged_filtered$activity_id[merged_filtered$activity_id==6] <- "LAYING"

#we turn the dataframe into a table to make group-by easier
me_fi <- data.table(merged_filtered)
#this is the final data set where we keep only the average grouped by the subject and the activity
final <- me_fi[,lapply(.SD, mean), by=list(subject_id,activity_id)]

#and then we write
write.table(final, file = "results.txt", row.names = FALSE)
