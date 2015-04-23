!READ MD FILES AS RAW!
===================================================================================
run_analysis.R Script:
-----------------------------------------------------------------------------------

The script must be run in R console or Rstudio console. The script , when run , takes input files from the 'UCI HAR dataset' folder which includes activity_labels, features, features_info and folders test and train containing X, y and subject text files each. The output of the script returns a new text file named 'Tidy Data.txt', which shows the mean values of the accelerometer and gyroscope calculations acquired that were acquired in each experiments.

NOTE-1: 
--------
*The user must set its working directory as 'UCI HAR dataset' for the script to work*
NOTE-2:
-------- 
*The final 'Tidy Data.txt' obtained must be read in R with read.table with parameters 'header=TRUE,check.names=FALSE' and with parameters 'header=TRUE, check.names=FALSE, sep="" when read.csv is used'*

*Required packages*:
-------------------- 
plyr, dplyr, qdap

===================================================================================

STEP-1: Merge training and test sets to create one data set
-X_test and X_train tests are read into Xtest and Xtrain objects and each of them have been concatednated with an ID, Subject and Data column. 
-ID column is for maintaining the order of the columns, the Subject column denotes 1 of 30 subjects for the experiment and the Data column is to denote experiment set ( test or train). 
-features text file contains the names of the features which are to be assigned as columns to the data sets of test and train. 
-Xtest and Ytest objects are merged to get the merged data set. The Merged data set is then ordered based on 'Data' and 'ID' columns

===================================================================================

STEP-2: Extract only the measurements on the mean and standard deviation for each measurement
-Merged data object 'mergeData' has column names 'ID', 'Subject', 'Data' and names of each features and these names are put into object text. 
-grep() is used to extract only those column names which have mean() and std() in them.

===================================================================================

STEP-3: Uses descriptive activity names to name the activities in the data set
-Activities : WALKING, WALKING UPSTAIRS, WALKING DOWNSTAIRS, SITTING, STANDING, LAYING are mapped with their corresponding values in the 'ytest' and 'ytrain' objects obtained on reading from their respective files as factors.

===================================================================================

STEP-4: Appropriately labels the data set with descriptive variable names
-'qdap' package comes into play here which includes the mgsub() used for replacing the required sub-strings. 
-'text' object contains the strings which are to be replaced and the 'replacement' object contains the corresponding replacements. 
-Corrected names are then assigned as names of the merged data set 'mergeData'.

===================================================================================

STEP-5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
-'dplyr' package comes into play here which has the 'group_by()', 'mutate()' and 'summarise_each()' functions. 
-merged data is grouped by Subject and Activity. 
-summarise_each() summarises each column based on groupings (Subject, Activity) and returns the mean of each experiment performed on each subject based on activity. 
-'Data' and 'ID' columns are later not required hence are removed. 
-'rwidth' object is used for assigning the widths of the columns. Width is set to 12 for first two columns and 25 for the rest of the columns.
-'Activity' column is then converted to character to avoid inconsistency in naming using mutate()(else would result to NA values).
-names of the columns are appended to the data set as the first column using cbind().
-format() is used to format the data set.
-Names of the columns are then removed so that the first column would provide as column names and the columns would be aligned this way.
-write.table() is then used to write the data set to 'Tidy Data.txt' file with quote=FALSE for disabling quotes and row.name=FALSE.
-user can read the 'Tidy Data.txt' file for his own purpose using parameter 'header=TRUE, check.names=FALSE,sep="" '.

===================================================================================
