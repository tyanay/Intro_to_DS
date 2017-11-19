#ID 1: 
#ID 2: 

## Download all relevant packages and libraries:


# 1.a. Download and extract the data from Moodle into a local folder designated for this assignment.

# 1.b. Set your working directory to be your assignment folder for easy access. 
# From now on, if needed, do not use the full path, but only the name of the file within this path.
################################

# 1.c. Import the CSV file " chicago_taxi_data.csv " into R and save it by the name data. 
# Notice the data in the file has row numbers that are redundant (so pay attention to the function arguments).
################################
data = read.csv("chicago_taxi_data.csv")
# 1.d Make sure the data was loaded properly by showing the first few rows of the data.
################################
head(data)

# 2.a Sample 10000 rows from the dataset without replacement. This file will be our dataset throughout the exercise. 
# Before you sample, set your random seed to be 1. 
################################
set.seed(1)
data_sample = data[sample(nrow(data), 1000), ]

# 2.b We will not use any of geographical columns (pickup/ dropoff - longtitude/ latitude). Delete these columns.
################################
colnames( data_sample)
drops <- c("pickup_latitude", "pickup_longitude", "dropoff_latitude", "dropoff_longitude")
data_sample <- data_sample[ , !(names(data_sample) %in% drops)]
colnames(data_sample)
# 2.c Show the names and the data type of all the features.
################################

# 2.d. The Column pickup_census_tract has only NA values. Create a verification check for this claim. 
#Delete the column pickup_census_tract and dropoff_census_tract from the dataset.
#Could we have known in advanced that this column is problematic? Tip: use your answer the previous question.
################################

# 2.e What's your opinion about the current type of the column 'company'? Is it adequate?
#If yes, explain why. It not, explain why not and change the type of this column and other similar columns.
################################

# 2.f. Create a summary statistics of the dataset (using one-line command). 
# What is the difference between the output for the numerical columns and the non-numeric columns?
################################


# 3.a. Calculate the percentage of rows with at least one missing value (NA)
################################

# 3.b. Delete all rows with more than 1 missing value (NA)
################################

# 3.c. Create a histogram of a categorical column (to your choice). Explain the findings of the chart. 
# Pay attention that some histogram functions work only with numerical values, so a transformation is needed.
################################

# 3.d. Choose and implement the best way to deal with the missing values in the dataset, in your opinion (according to your previous findings). 
# As for the columns: [trip_seconds, trip_miles, trip_total] , deal with 0's (zeros) as if they were NA's .
#Pay attention - you can decide to delete specific rows or columns, while impute some other remaining missing values. Explain all of your choices.
################################


# 4.a. Make a Q-Q plot for each of the following columns: [trip_seconds, trip_miles, trip_total]. 
# Explain what we can learn from a Q-Q plot about the distribution of the data.
################################

# 4.b. (7) According to the Q-Q plots ,do we need to normalize these features? Which normalization function should we use for each feature, if any?
# For each feature, in case you decided to normalize it, create a new normalized column of the feature (eg. norm.trip_seconds).
################################


# 5.a. Create a boxplot of the normalized trip_miles column (or the original column in case you chose not to normalize) Remove the column's 
# outliers from the data based on the box plot. Hint: use the boxplot object.
################################

# 5.b. Implement a min-max transformation on the normalized columns of [trip_seconds, trip_miles, trip_total] 
# (or the original columns in case you chose not to normalize). 
# Create new column with the transformed data (eg. minmax.trip_seconds) 
################################


#6.a. Create a correlation matrix of all the relevant numerical features. In addition, Display a correlation plot for this matrix. 
# Write 3 business insights we can learn from the correlation matrix.
################################

#6.b. Create 5 different statistical outputs based on the dataset. Visualize at least 3 of them. Add an explanation. Try to be creative.
#Examples:
#  1.	A bar chart that displays the average and median amount of trip_total, for each payment_type. 
#  2. Density plots of trip_second - one for each day.
################################