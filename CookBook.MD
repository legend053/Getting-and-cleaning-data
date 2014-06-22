Cookbook for finishing the course project
Source data
Human Activity Recognition Using Smartphones Dataset

Purpose of this dataset
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

Dataset categories:
Train
Test

Common features (variables recorded):

The complete list of variables of each feature vector is available in 'features.txt'

In this project, the variables containing std() or mean() will be picked up for studying.

Dataset structures
The structures of train and test are the same. They are consititued by three files which are subject_train/test, X_train/test, y_train/test.

To study the two datasets, the seperate files needed to be put together to find the relationship among different subject, activities and recorded variables as shown in 'feature.txt'.

Step 1: Combine train datasets and test datasets seperately.
  1. Read data by read.table command
  2. name each variable for future organizition.
  3. Create a column called 'ID' for merging subject,X and Y together.
  4. Apply merge command to realize the merge function
  5. repeate 1~4 for test datasets

Step 2: Performe primary process
  1. Determine the variables needed to be extracted.
    In this step, grepl function is needed.
  2. repeate 1 for test datasets
  
Step 3: Merges the training and the test sets to create one data set
  1. By rbind command, train and test are combined.

Step 4: Uses descriptive activity names to name the activities in the data set
  1. For loop can be applied
  
Step 5: Appropriately labels the data set with descriptive variable names
  1. assign the current total datasets' names to a variable
  2. create a function by repeating applying gsub function
  3. by using the created function to finish the objective
  
Step 6: Generate the first tidy data without furture process
  1. by write.csv function
  
Step 7: Generate the second tidy data with applying mean function
  1. Install the package reshape2
  2. by applying melt and dcast functions in sequence to finish the last objective.
  3. by write.csv function
  
After finishing the above seven steps,the course project is finished as expected.
