##### run_analysis.R script file creates tidy data
##### for more info see README.txt file
##### and CodeBook.md for variable descriptions

# Load appropriate packages

if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")





### 1. Merge the training and the test sets to create one data set ###
######################################################################


# Load activity_labels (6 activities) and features (561 feature vector)
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
activity_labels
features <- read.table("./UCI HAR Dataset/features.txt")[,2]


#### Load test data (30% of sample) ####


# X_test & y_test (X_test subject labels) data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
dim(X_test)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
dim(y_test)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
dim(subject_test)

dim(features)

# Add feature labels to X_test
names(X_test) = features



#### Load Training data (70% of sample) ####


# X_train & y_train (X_train subject labels data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
dim(X_train)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
dim(y_train)

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
dim(subject_train)


# Add features labels to X_train
names(X_train) = features


### 2. Extracts only the measurements on the mean and standard deviation for each measurement ###
#################################################################################################

# Extract only the variables with mean and standard deviation 
# from the X_test dataset

extract_features <- grepl("mean|std", features) #create a T/F for a list with mean and std in the text
X_test = X_test[,extract_features] # subset to have only those variables

# Load activity_labels
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"

# Combine data
test_data <- cbind(as.data.table(subject_test), y_test, X_test) # Test data ready


# Extract only the variables with mean and standard deviation 
# from the X_train dataset

X_train = X_train[,extract_features]

# Load activity_labels
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

# Combiine data
train_data <- cbind(as.data.table(subject_train), y_train, X_train) # Train data ready

# Merge test and train data together
final_data = rbind(test_data, train_data) # Items 1 and 2 completed



### 3. Uses descriptive activity names to name the activities in the data set ###
### 4. Appropriately labels the data set with descriptive variable names.     ###
#################################################################################

# Extract variable names from the final_data 
variable_names <- names(final_data)

# Make the variables more readable 
new_variable_names <- sub("Acc", "Accelerometer", variable_names)
new_variable_names <- sub("Gyro", "Gyroscope", new_variable_names)
#new_variable_names <- sub("()", "", new_variable_names)


names(final_data) <- new_variable_names



### 5. From the data set in step 4, creates a second,                  ###
###    independent tidy data set with                                  ###  
###    the average of each variable for each activity and each subject ###
##########################################################################

# The following reshaping makes the data more visually readable
# by utilizing mend and dcast functions

id_labels   = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(final_data), id_labels)
melt_data      = melt(final_data, id = id_labels, measure.vars = data_labels)

# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)



write.table(tidy_data, file = "./tidy_data.txt")
