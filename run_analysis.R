#Merge the training and the test sets to create one data set.
#-------------------Read main datasets (i.e., sin headings) into R-------------------------------
## Test data
X_test_data <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "\t")
y_test_data <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "\t")
subject_test_data <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "\t")
## Train data
X_train_data <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "\t")
y_train_data <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "\t")
subject_train_data <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "\t")
#-------------------Altering main datasets for use -----------------------
## Upon inspection, I notice that the X data - for both test and train - appears to need to be further split 
## so I will split the rows using strsplit().
## I use regular expression and strsplit to split V1 into 561 columns (the number indicated by the experiment)
split_X_test_data <- data.frame(do.call(rbind, strsplit(X_test_data$V1, "(?<=e-00[0-9])", perl = TRUE)))
split_X_train_data <- data.frame(do.call(rbind, strsplit(X_train_data$V1, "(?<=e-00[0-9])", perl = TRUE)))

#-------------------Merging all main datasets together--------------------
## Stack X_test on top of X_train (using the split ones with 561 variables)
stacked_X_data <- rbind(split_X_test_data, split_X_train_data)
## Stack y_test on top of y_train
stacked_y_data <- rbind(y_test_data, y_train_data)
## Stack subject test on top of stubject train data
stacked_subject_data <- rbind(subject_test_data,subject_train_data)
## Put subject data in far left column, y data in next column, and then the X data on the right of those
merged_data <- cbind(stacked_subject_data,stacked_y_data,stacked_X_data)
## Clear/remove all objects except merged_data from the global environment to keep things more clean in here
rm(list = setdiff(ls(), "merged_data"))
## Save merged_data as a file that I can retrieve later without running everything above multiple times
saveRDS(merged_data, file = "merged_data.RDS")

#-------------------Adding headings---------
## Read in the data indicating column names for V3:V563
column_names <- read.table("UCI HAR Dataset/features.txt", header = FALSE, sep = "\t")
colnames(merged_data)[3:563]<-column_names$V1
## Assign column names to the first two variables
colnames(merged_data)[1]<-"Subject_ID"
colnames(merged_data)[2]<-"Activity_Name"
## Clear/remove all objects except merged_data from the global environment to keep things more clean in here
rm(list = setdiff(ls(), "merged_data"))
## Save new merged_data as a file that I can retrieve later without running everything above multiple times
saveRDS(merged_data, file = "merged_data.RDS")

#-------------------Use descriptive activity names to name the activities in the data set------------
## Step 1: Read in txt file that tells you what each activity number corresponds to which activity
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = " ")

## Step 2: Manually name the columns of the activity_labels data frame
colnames(activity_labels) <- c("Activity_Num", "Activity_Name")

## Step 3: Create a named vector to map activity numbers to labels
activity_mapping <- with(activity_labels, setNames(Activity_Name, Activity_Num))

## Step 4: Convert the "Activity Name" column to a factor with labels
merged_data$Activity_Name <- factor(merged_data$Activity_Name, labels = activity_mapping)

## Clear/remove all objects except merged_data from the global environment to keep things more clean in here
rm(list = setdiff(ls(), "merged_data"))

## Save new merged_data as a file that I can retrieve later without running everything above multiple times
saveRDS(merged_data, file = "merged_data.RDS")

#-------------Extract only the measurements on the mean and standard deviation for each measurement.------------ 
## Select columns with names containing "mean()" or "std()"
selected_cols <- grep("mean\\(\\)|std\\(\\)", names(merged_data), value = TRUE)
main_data <- merged_data[, c("Subject_ID", "Activity_Name", selected_cols)]
## Save data so I don't need to reload everything if I have to stop and come back 
saveRDS(main_data, file = "main_data.RDS")
## Clear the workspace of everything except the main data
rm(list = setdiff(ls(), "main_data"))

#Appropriately label the data set with descriptive variable names 
## Without more information provided by the instructors, I am not going to waste my time making up new names.
#-----------From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.--
## Load the dplyr package
library(dplyr)

## Convert columns 3 to 68 to numeric
main_data <- mutate(main_data, across(3:68, as.numeric))

## Group by Subject_ID and Activity_Name, and calculate the mean of each variable
new_dataset <- main_data %>%
  group_by(Subject_ID, Activity_Name) %>%
  summarise(across(everything(), mean))
## Save small data so I don't need to reload everything if I have to stop and come back 
saveRDS(new_dataset, file = "new_dataset.RDS")
