#ID 1: 
#ID 2: 

## Download all relevant packages and libraries:
#install.packages("DMwR")
#install.packages("corrplot")

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
data_sample = data[sample(nrow(data), 10000), ]

# 2.b We will not use any of geographical columns (pickup/ dropoff - longtitude/ latitude). Delete these columns.
################################
colnames( data_sample)
drops <- c("pickup_latitude", "pickup_longitude", "dropoff_latitude", "dropoff_longitude")
data_sample <- data_sample[ , !(names(data_sample) %in% drops)]
colnames(data_sample)
# 2.c Show the names and the data type of all the features.
sapply(data_sample, class)
################################

# 2.d. The Column pickup_census_tract has only NA values. Create a verification check for this claim. 
#Delete the column pickup_census_tract and dropoff_census_tract from the dataset.
#Could we have known in advanced that this column is problematic? Tip: use your answer the previous question.
################################
levels(data_sample$pickup_census_tract)
data_sample <- data_sample[, !(names(data_sample) %in% c("pickup_census_tract","dropoff_census_tract"))]
names(data_sample)

# We could have known that the column might be problametic, thank to the check we did to the data types of the column.
# The Pickup Census Tract is expected to be a string (names of the census tracts), so we excpected a "factor" type column (the column was "numeric").


# 2.e What's your opinion about the current type of the column 'company'? Is it adequate?
#If yes, explain why. It not, explain why not and change the type of this column and other similar columns.
################################

levels(data_sample$company)
# The 'company' column is currently "numeric". This might be a problem, becuase the clasiffier might give a meaning to the numeric values of this column,
# when it acctually represents an identifier for a taxi company.
# Our assumption is that there no numeric meaning to the company ID code, and this is used only as an identifier.

data_sample$company <- factor(data_sample$company)
levels(data_sample$company)

#and the same for similar columns
data_sample$pickup_community_area <- factor(data_sample$pickup_community_area)
data_sample$dropoff_community_area <- factor(data_sample$dropoff_community_area)
data_sample$taxi_id <- factor(data_sample$taxi_id)


# 2.f. Create a summary statistics of the dataset (using one-line command). 
# What is the difference between the output for the numerical columns and the non-numeric columns?
################################
summary(data_sample)

# Numircal - the output includes Mean, Min, Max, 1st, 2nd (median) and 3rd Qu. The non-numerical includes only the number of instances of the factor levels.


# 3.a. Calculate the percentage of rows with at least one missing value (NA)
################################
sum(!complete.cases(data_sample)) / nrow(data_sample)

# 3.b. Delete all rows with more than 1 missing value (NA)
################################
data_sample = data_sample[rowSums(is.na(data_sample)) < 2, ]

# 3.c. Create a histogram of a categorical column (to your choice). Explain the findings of the chart. 
# Pay attention that some histogram functions work only with numerical values, so a transformation is needed.
################################

barplot(table(data_sample$payment_type))

# From the given chart, we can understand that there are almost no occurences for payment types that are other than Cash or Credit Card.
# Cash is definetly the most common way for paying.

# 3.d. Choose and implement the best way to deal with the missing values in the dataset, in your opinion (according to your previous findings). 
# As for the columns: [trip_seconds, trip_miles, trip_total] , deal with 0's (zeros) as if they were NA's .
# Pay attention - you can decide to delete specific rows or columns, while impute some other remaining missing values. Explain all of your choices.
################################
colSums(is.na(data_sample))


############# Company column
length(data_sample$company[is.na(data_sample$company)]) / length(data_sample$company)
# More than third of the data in the data sample is with missing company ID, and NA is the largest factor in this column.
# We can visualize this with the following barplot (notice the left bar):
barplot(table(data_sample$company, useNA = c("ifany")))

# In the following code, we will try to fill the NAs in the company column for taxis that appear somewhere else in the dataframe with a company.
# Maybe in one row, some taxi_id has an NA value for company, but in another row, the same taxi_id has a company.

# take the unique values of taxi and company
df_taxi <- unique(data_sample[!is.na(data_sample$company), c('taxi_id', 'company')])

# Remove all the taxi_id that has more than 1 company related to them:
df_taxi2 <- aggregate(data_sample$company, list(taxi_id=data_sample$taxi_id), FUN= function(x) length(x))
df_taxi2 <- df_taxi2[df_taxi2$x < 2,'taxi_id']
df_taxi <- df_taxi[(df_taxi$taxi_id %in% df_taxi2),]

# Fill the missing NAs in the data_sample with the relevant company from the df_taxi
data_sample$company[is.na(data_sample$company)] <- df_taxi$company[match(data_sample$taxi_id[is.na(data_sample$company)], df_taxi$taxi_id)]

# Check if the amount of NAs has changed:
length(data_sample$company[is.na(data_sample$company)]) / length(data_sample$company)
# Unfortunately, the amount of NAs did not change.
# We will check if there are taxis without companies that appear in the new taxi-company dataframe we created 
df_taxi[df_taxi$taxi_id %in% data_sample[is.na(data_sample$company) ,c('taxi_id')], ]
# 0 matches - this explains the results (no NA filled).

# Conclusion - We wish to keep this data due to its large scale, and because there might be a meaning to the "NA" value in the company column (e.g. independant drivers without a company).
# We have decided to create a new category for the NA rows (Company = "NA").
data_sample$company <- addNA(data_sample$company)
summary(data_sample$company) # Validation


########### pickup_community_area
# there are only 15 rows without pickup community area value. This is 0.15% of the data. We have decided to remove those rows due to the small scale.
data_sample <- data_sample[!is.na(data_sample$pickup_community_area),]

########### dropoff_community_area
#lets give a look on the data:
data_sample[is.na(data_sample$dropoff_community_area),]
#we see that there are a lot of rows where both "trip_miles"=0 and "dropoff_community_area=Na" we want to remove these rows:
data_sample <- data_sample[!(is.na(data_sample$dropoff_community_area) & (data_sample$trip_miles==0)),]
summary(data_sample$dropoff_community_area) #now we have just 55 NAs
#we assume that short trips probably stay in the same community area (dropoff_area=pickup_area) we want to check what is "short" in point of view of our data
same_area_trips <- data_sample[data_sample$pickup_community_area==data_sample$dropoff_community_area & !is.na(data_sample$dropoff_community_area),]
same_area_trips
mean(same_area_trips$trip_miles)
median(same_area_trips$trip_miles)
#we see that in our data the mean and the median of same area trips are: 0.9 and 0.6, we will use the median (0.6) as a threshold
data_sample[is.na(data_sample$dropoff_community_area) & data_sample$trip_miles<=0.6,]$dropoff_community_area <- data_sample[is.na(data_sample$dropoff_community_area) & data_sample$trip_miles<=0.6,]$pickup_community_area
summary(data_sample$dropoff_community_area) #now we have just 46 NAs
#we assume that long trips probably go out from Chicago we want to check what is "long" in point of view of our data
#on a map of Chicago we can see that the size of the city in miles is apro 25x5 (except area 76- that is not connected to the rest of the city)
#reference to the Chicago community areas map:https://en.wikipedia.org/wiki/Community_areas_in_Chicago
# lets see if we can find trips above 25 miles that not include area 76:
data_sample[data_sample$trip_miles>25.0 & data_sample$pickup_community_area!='76' & data_sample$dropoff_community_area!='76' & !is.na(data_sample$dropoff_community_area),]
#we found 11 trips- so the assumption that trips above 25 miles going out of the city is wrong
#we have decided to remove the 46 rows that stay with NA value in the dropoff_community_area, we can't impute those values without more information on the domain
# impute with probability or the most freaquent value make no sense on this case.
data_sample <- data_sample[!is.na(data_sample$dropoff_community_area),]
data_sample[is.na(data_sample$dropoff_community_area),] #validation

#### Taxi_id
data_sample[data_sample$taxi_id==0,]
# we have only 2 rows with no taxi id (equls to zero).
# we will remove these rows
data_sample <- data_sample[!data_sample$taxi_id==0,]

#### trip_total and fare:
# check the rows with trip_total = 0
data_sample[data_sample$trip_total==0,]
# only 5 rows, with unreliable data (diffrences between times, community areas). remove these 5 rows:
data_sample <- data_sample[!data_sample$trip_total==0,]
# by that, we have also dealt with the 0 fare coloumn:
data_sample[data_sample$fare==0,]

### Trip seconds and trip miles.

# accourding to the CITY OF CHICAGO TAXICAB FARE RATES and INFORMATION,
# the fare rate for a ride in 2016 is:
#     Flag Pull (Base Fare)	$ 3.25
#     Each additional mile	$ 2.25
#     Every 36 seconds of time elapsed	$ 0.20
# reference: https://www.cityofchicago.org/city/en/depts/bacp/supp_info/2012_passenger_information.html

# This information will help us in 2 ways:
# 1.  A "trip" that took 0 seconds, and 0 miles, and its fair is 3.25$, means that there was no actual trip.
#     we will remove these rows:
data_sample <- data_sample[!((data_sample$trip_seconds==0) & (data_sample$trip_miles==0) & (data_sample$fare==3.25)),]

# 2.  For Rows that have 0 miles, or 0 time (but not both!), and has fair, we can calculate the diffrences using this formula:
#     fare = 3.25 + trip_miles * 2.25 + floor(trip_seconds / 36) * 0.20
#       a. trip_miles = (fare - 3.25 - floor(trip_seconds/36)*0.20) / 2.25  ** this shall be rounded by 0.1 (liek the original data)
#       b. trip_seconds = (fare - 3.25 - trip_miles * 2.25 ) * 36 / 0.20 + 18 ** notice that this will not be an exact match due to the floor function, we have added 18, which is the median between 0 and 36

# a. trip_miles:

nrow(data_sample[data_sample$trip_miles==0 & data_sample$trip_seconds!=0, ])

data_sample <- data_sample[!is.na(data_sample$X), ]  # remove NA values (if there are any...)

data_sample[data_sample$trip_miles==0 & data_sample$trip_seconds!=0, ]$trip_miles <- round((data_sample[data_sample$trip_miles==0 & data_sample$trip_seconds!=0, ]$fare - 3.25 - floor(data_sample[data_sample$trip_miles==0 & data_sample$trip_seconds!=0, ]$trip_seconds / 36) * 0.2) / 2.25 , 1)

nrow(data_sample[data_sample$trip_miles<=0 & data_sample$trip_seconds!=0, ])  # we have calculated the trip miles for ~1200 rows!

# remove rows that equals 0 or less:
data_sample <- data_sample[!(data_sample$trip_miles<=0), ]


# b.trip_seconds:
data_sample[data_sample$trip_seconds==0, ]$trip_seconds <- round(( data_sample[data_sample$trip_seconds==0, ]$fare - 3.25 -data_sample[data_sample$trip_seconds==0, ]$trip_miles * 2.25 ) * 36 / 0.2 + 18)
# remove the data with seconds equals 0 or less
data_sample <- data_sample[data_sample$trip_seconds > 0, ]


# 4.a. Make a Q-Q plot for each of the following columns: [trip_seconds, trip_miles, trip_total]. 
# Explain what we can learn from a Q-Q plot about the distribution of the data.
################################

###trip_miles distribution:
qqnorm(data_sample$trip_miles); qqline(data_sample$trip_miles) ##clearly the data not from a "Normal" distribution
#trying to understand the distribution
hist(data_sample$trip_miles)
hist(data_sample[data_sample$trip_miles<120,]$trip_miles)# lets take a closer look
hist(data_sample[data_sample$trip_miles<20,]$trip_miles) # and closer

###trip_seconds distribution:
qqnorm(data_sample$trip_seconds); qqline(data_sample$trip_seconds) #clearly the data not from a "Normal" distribution
# trying to understand the distribution:
hist(data_sample$trip_seconds) 
hist(data_sample[data_sample$trip_seconds<3000,]$trip_seconds) # lets take a closer look  
hist(data_sample[data_sample$trip_seconds<1500,]$trip_seconds) # and closer
###trip_total distribution:
qqnorm(data_sample$trip_total); qqline(data_sample$trip_total)
#We can see that almost all the data appears on the "Normal line"
hist(data_sample$trip_total)
hist(data_sample[data_sample$trip_total<500,]$trip_total) # lets take a closer look  
hist(data_sample[data_sample$trip_total<15,]$trip_total) # a look to the "center of mass" of the distribution- now it looks closer to "Normal"
# we decided that its close enogh- we will manage the "total_trip" as a Normal distribution

# 4.b. (7) According to the Q-Q plots ,do we need to normalize these features? Which normalization function should we use for each feature, if any?
# For each feature, in case you decided to normalize it, create a new normalized column of the feature (eg. norm.trip_seconds).
################################
#We need to normalize the numeric features becuase we don't want that the model we gonna use will give a meaning to the "scale" of the data (e.g' seconds, Km, $)
#for the "trip_total" column we will use Z transformation becuase its close enogh to Normal
#for trip_miles and trip_seconds we will use another scaling that not based on "Normal" assume

###normalize trip_total:
data_sample['norm.trip_total'] <- scale(data_sample$trip_total) #scale is a function that return the standardized scores of a vector
data_sample['log.trip_total'] <- log(data_sample$trip_total)
###normalize trip_miles
#due to 5.b question we will use here also in "scale", and in question 5.b we will use min-max transformation
data_sample['norm.trip_miles'] <- scale(data_sample$trip_miles)
###normalize trip_seconds
#due to 5.b question we will use here also in "scale", and in question 5.b we will use min-max transformation
data_sample['norm.trip_seconds'] <- scale(data_sample$trip_seconds)

# 5.a. Create a boxplot of the normalized trip_miles column (or the original column in case you chose not to normalize) Remove the column's 
# outliers from the data based on the box plot. Hint: use the boxplot object.
################################
boxplot(data_sample$norm.trip_miles)['out']
data_sample <- data_sample[!(data_sample$norm.trip_miles %in% (boxplot(data_sample$norm.trip_miles, outline = FALSE)$out)),]
summary(data_sample$norm.trip_miles)

# 5.b. Implement a min-max transformation on the normalized columns of [trip_seconds, trip_miles, trip_total] 
# (or the original columns in case you chose not to normalize). 
# Create new column with the transformed data (eg. minmax.trip_seconds) 
################################
data_sample['minmax.norm.trip_miles'] <- as.vector((data_sample$norm.trip_miles- min(data_sample$norm.trip_miles))/(max(data_sample$norm.trip_miles)-min(data_sample$norm.trip_miles))) 
data_sample['minmax.norm.trip_seconds'] <- as.vector((data_sample$norm.trip_seconds- min(data_sample$norm.trip_seconds))/(max(data_sample$norm.trip_seconds)-min(data_sample$norm.trip_seconds))) 
data_sample['minmax.norm.trip_total'] <- as.vector((data_sample$norm.trip_total- min(data_sample$norm.trip_total))/(max(data_sample$norm.trip_total)-min(data_sample$norm.trip_total))) 


#5.c. Using the 3 columns you created, you will use a hierarchical-clustering method, followed by density-based method.  
#First, use hierarchical-clustering method to evaluate the probability of each instance to be an outlier. 
#Exclude all instances with 0.75 chance or higher. Hint: use "DMwR" package. 
#Then, using LOF, pick k=10 and remove all instances that their LOF score is above 1.4. Hint: use "Rlof" package.

###using hierarchical-clustering method to evaluate the probability of each instance to be an outlier
library(DMwR)
minmax_columns <- c("minmax.norm.trip_seconds","minmax.norm.trip_miles","minmax.norm.trip_total")
out <- outliers.ranking(data=data_sample[,minmax_columns],test.data=NULL,clus=list(dist='euclidean',alg='hclust',meth='average'))
out$prob.outliers

###Exclude all instances with 0.75 chance or higher
data_sample <- data_sample[out$prob.outliers<0.75,]
nrow(data_sample)

#using LOF to remove all instances that their LOF score is above 1.4 (k=10)
data_sample <- data_sample[lofactor(data_sample[,minmax_columns], k=10)<=1.4,]
nrow(data_sample)

#6.a. Create a correlation matrix of all the relevant numerical features. In addition, Display a correlation plot for this matrix. 
# Write 3 business insights we can learn from the correlation matrix.
################################
sapply(data_sample, class)
# now "tolls" contains just "0", we decided to remove it.
data_sample$tolls <- NULL

### Corrwlation metrix of all the numeric features (for trip_miles, trip_seconds and trip_total we use just
### the minmax columns):
cor_columns <- c("fare", "tips", "extras", "minmax.norm.trip_seconds","minmax.norm.trip_miles","minmax.norm.trip_total")

data_cor <- cor(data_sample[, cor_columns])
data_cor

###plot the correlation:
library(corrplot)
corrplot(data_cor, method = "circle")

###insights:
#1. tips are not correleted to distance and time- driver that want enlarge the tips ammount shouldn't
#looking for long rides
#2. as expected, fare highly connected to time and distnce of the trip- the fare mainly calculats from
#distance and time
#3. the total cost is highly correleted with the fare (0.85)- most of the total cost is the fare.


#6.b. Create 5 different statistical outputs based on the dataset. Visualize at least 3 of them. Add an explanation. Try to be creative.
#Examples:
#  1.	A bar chart that displays the average and median amount of trip_total, for each payment_type. 
#  2. Density plots of trip_second - one for each day.
################################