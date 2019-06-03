#################################
#     File: run_analysis.R     #
###############################


#   Overview: Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#
#   See README.md for details.


library(dplyr)

################################################
# PART 1: Get Data.
###############################################


url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "UCI HAR Dataset.zip"

if (!file.exists(file)) {
  download.file(url, file, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(file)
}


#################################################
#  PART2: Read Data.
################################################


# reading training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))


# reading test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# reading features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)


# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


##############################################################################
# STEP 1: Merge the training and the test sets to create one data set.
##############################################################################


# concatenating individual data tables into a single data table
bigTable <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)


# removing individual data tables
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)


# assigning column names
colnames(bigTable) <- c("subject", features[, 2], "activity")



##############################################################################
# STEP 2: Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################


# determining columns of data set to keep based on column names ..
columnsToKeep <- grepl("subject|activity|mean|std", colnames(bigTable))

# .. and keeping data in these columns only
bigTable <- bigTable[, columnsToKeep]




##############################################################################
# STEP 3: Use descriptive activity names to name the activities in the data
#          set
##############################################################################


# replace activity values with previously named factor levels
bigTable$activity <- factor(bigTable$activity, 
                                 levels = activities[, 1], labels = activities[, 2])



##############################################################################
# STEP 4: Appropriately label the data set with descriptive variable names
##############################################################################


# getting all column names
bigTableCols <- colnames(bigTable)


# removing special characters and punctuation marks
bigTableCols <- gsub("[[:punct:]]", "", bigTableCols)


# expanding abbreviations and cleaning up names
bigTableCols <- gsub("^f", "frequencyDomain", bigTableCols)
bigTableCols <- gsub("^t", "timeDomain", bigTableCols)
bigTableCols <- gsub("Acc", "Accelerometer", bigTableCols)
bigTableCols <- gsub("Gyro", "Gyroscope", bigTableCols)
bigTableCols <- gsub("Mag", "Magnitude", bigTableCols)
bigTableCols <- gsub("Freq", "Frequency", bigTableCols)
bigTableCols <- gsub("mean", "Mean", bigTableCols)
bigTableCols <- gsub("std", "StandardDeviation", bigTableCols)


# correcting repeated words
bigTableCols <- gsub("BodyBody", "Body", bigTableCols)

# using new labels as column names
colnames(bigTable) <- bigTableCols


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################


# grouping by subject and activity and summarising using mean
bigTableMeans <- bigTable %>% 
  group_by(subject, activity) %>%
  summarise_each(list(mean))


# saving output to file "tidy_data.txt"
write.table(bigTableMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

