# **Getting and Cleaning Data** course project
The purpose of this project is to collect, work with, and clean a data set. 

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
Through this project work, the open data set collected from the accelerometers from the Samsung Galaxy S smartphone was cleaned to finally obtain a new tidy data which can be used for later analysis. Details of the original data set is as written in the 'Data source' section below. 

This repository contains all files related to this project as follows:

* `README.md`: (This file) Describes what each files are for.
* `Codebook.md`: Describes the variables, the data, and any transformations made to clean up the data
* `tidydata.txt`: The output data set created with step 6 of run_analysis.R
* `run_analysis.R`: The R script used to clean up the data to achieve the objectives below:
1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement.
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive variable names.
5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
6. Finally output the result to a new file (tidydata.txt).

# Data source 
A full description of the source dataset is available at the original project website:
[UCI](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)


# Environments
This project was done with:
* Windows 7 SP1 64-bit
* R version 3.3.3
* Rstudio 1.0.136
* R packages: dplyr 0.5.0
