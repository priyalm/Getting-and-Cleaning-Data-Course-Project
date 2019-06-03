# Coursera *Getting and Cleaning Data* course project

In this project, data collected from the accelerometer and gyroscope of the Samsung Galaxy S smartphone was retrieved, worked with, and cleaned, to prepare a tidy data that can be used for later analysis.

This repository contains the following files:

- `README.md`, this file, which provides an overview of the data set and how it was created.
- `tidy_data.txt`, which contains the data set.
- `CodeBook.md`, the code book, which describes the contents of the data set (data, variables and transformations used to generate the data).
- `run_analysis.R`, the R script that was used to create the data set (see the [Creating the data set](#creating-data-set) section below) 

## Study design <a name="study-design"></a>

The source data set that this project was based on was obtained from the [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#), which describes how the data was initially collected as follows:


Training and test data were first merged together to create one data set, then the measurements on the mean and standard deviation were extracted for each measurement (79 variables extracted from the original 561), and then the measurements were averaged for each subject and activity, resulting in the final data set.

## Creating the data set <a name="creating-data-set"></a>

The R script `run_analysis.R` can be used to create the data set. It retrieves the source data set and transforms it to produce the final data set by implementing the following steps (see the Code book for details, as well as the comments in the script itself):

- Download and unzip source data if it doesn't exist.
- Read data.
- Merge the training and the test sets to create one data set.
- Extract only the measurements on the mean and standard deviation for each measurement.
- Use descriptive activity names to name the activities in the data set.
- Appropriately label the data set with descriptive variable names.
- Create a second, independent tidy set with the average of each variable for each activity and each subject.
- Write the data set to the `tidy_data.txt` file.

The `tidy_data.txt` in this repository was created by running the `run_analysis.R` script.

This script requires the `dplyr` package.