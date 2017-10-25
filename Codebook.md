# Code book for Getting and Cleaning Data course project
This codebook connects to the data file `tidydata.txt` in this repository. It consists of 180 rows and 68 columns, where the first row contains the variable names.

## Data
The data file `tidydata.txt` was created as a tidy data set through the project work with `run_analysis.R`, to clean the data source acquired from here: [UCI Machine learning project data](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

The original data source is a set of measurement of 6 activities performed by 30 individuals, which were obtained with smartphones. These data include:
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration
- Triaxial angular velocity from the gyroscope

This original data set consists of training and test data sets. These data sets were cleaned as a tidy data set with the average of each variable for each activity and each subject. Transformation details are described in the next section ("Transformations"). 

## Transformations
The transformations made with the source data set through the project script(`run_analysis.R`) are as follows: 
#### Combine the original data set  
Step1: Merges the training and the test data sets to create one data set.
#### Extract specific variables  
Step2: Extracts only the variables on the means and standard deviations for each measurement.
#### Alter the value labels and variable names  
Step3: Applies descriptive activity names to values instead of factor numbers to express each activities in the data set.  
Step4: Appropriately re-labels the data set with descriptive variable names.
#### Summarize the data  
Step5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  
Step6: Outputs and saves the result to 'tidydata.txt'.

## Variables
All measurements are normalized and bounded within [-1,1].
### Units
The linear acceleration measurements which are derived from the signals captured with smartphone accelerometer are recorded in standard gravity units 'g'.
The angular velocity vector measured by the gyroscope are recorded in radians per second.
Jerk signals are time derivatives of the body linear acceleration or angular velocity, and the units are g/second and radians/second^3, respectively.
### Identifiers
* `Activity`
The activity type performed by the individual, strings of either 6 values (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING).
* `Subject`
The ID of each individual subjects, integers ranging from 1 to 30.
### Measurement variables
All values for the measured variables are numeric, floating-point number. The variables are categorized into two types, time domain signals and frequency domain signals. 
#### 1. Time domain signals  
Time domain signals were captured with the accelerometers and gyroscopes and recorded as 3-axial raw signals, and then subjected to certain calculations. Variables of time domain signals have prefix 'TimeDomain' in its variable names. Each abbreviation in the variables denotes the masurement features as below:  
  
**Measurement types; `Acc` or `Gyro`**
* `Acc`: The linear acceleration measurements calculated from signals captured with the accelerometers  
* `Gyro`: The angular velocity vector measurements calculated from signals captured with the gyroscopes  

**Body or Gravity acceleration signals; `Body` or `Gravity`**
* `Body`: The body acceleration signals separated with a low pass Butterworth filter from the original acceleration signals  
* `Gravity`: The gravity acceleration signals separated with a low pass Butterworth filter from the original acceleration signals  

**Jerk signal; `Jerk`**
* `Jerk`: Jerk signals obtained as a time derivative of the body linear acceleration and angular velocity

**Element types; `Magnitude`, `X`, `Y`, or `Z`**
* `Magnitude`: The magnitude of the three-dimensional signals, which were calculated using the Euclidean norm
* `X`, `Y` or `Z`: Signals in each orientation

**Descriptive statistics values; `Mean` or `Std`**
* `Mean`: The average of each measurement set
* `Std`: The stantdard deviation of each measurement set

#### 2. Frequency domain signals  
Frequency domain signals were calculated with Fast Fourier Transform (FFT) being applied to some of the time domain signal measurements. These are indicated in its variable name with prefix 'FrequencyDomain'. The abbreviations used in each variables are same as those used for time domain signals.
