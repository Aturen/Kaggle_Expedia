###################################################################################
## Initial viewing of Expedia Hotel Recommender data from Kaggle, April 2016
###################################################################################

##  LIBRARIES  ##
library(data.table)  # for fread to read large files, may need to install first
library(ggplot2)     # data visualization
library(randomForest)
##TEST
## DATA EXPLORATION AND CLEANING
## load the Expedia data in R
## Be sure your working directory is set to the cloned Expedia_Kaggle base directory
expedia.data <- fread("data_Known.csv", header=TRUE)        # ~3min to load
expedia.predict <- fread("data_Predict.csv", header=TRUE)   # ~10s  to load

## explore the data set
dim(expedia.data)
dim(expedia.predict)
str(expedia.data)
#summary(expedia.data)

## Clean both sets simultaneously
# Add variables hotel_cluster & others to the "test" dataset (gets removed later)
expedia.predict$is_booking <- 2
expedia.predict$cnt <- 0
expedia.predict$hotel_cluster <- 0
expedia.data$id <- -1

# Select out only booking events from train data
expedia.booked <- expedia.data[expedia.data$is_booking == 1]
dim(expedia.booked)

# Combine Kaggle's "train" and "test" datasets, name all_data.  
#  (We'll split into two groups after filling in missing values and tidying up)
all_data <- rbind(expedia.predict,expedia.booked)


##  VISUALIZATION  ##
# On cell phone?
ggplot(data = all_data, aes(all_data$is_mobile)) + geom_histogram()
ggplot(data = expedia.booked, aes(expedia.booked$is_mobile)) + geom_histogram()
# A: Most people are on the web, some are on their phones.  How does this change if they booked?

# Which hotel cluster did people book?
ggplot(data = expedia.booked, aes(expedia.booked$hotel_cluster)) + geom_histogram()

# Where did people book
ggplot(data = expedia.booked, aes(expedia.booked$hotel_continent)) + geom_histogram()
ggplot(data = expedia.booked, aes(expedia.booked$hotel_country)) + geom_histogram()

# Is there correlation between user_location_country and hotel_country?  A: NOOOOO
ggplot(data = expedia.booked, aes(x = hotel_country, y = user_location_country)) + geom_point()

# What days of the week are people booking?
expedia.booked$DayOfWeek <- factor(weekdays(as.Date(expedia.booked$date_time)), levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data = expedia.booked, aes(expedia.booked$DayOfWeek)) + geom_bar()

# Gets time: as.POSIXct(head(expedia.booked$date_time))


# Which are the most popular hotel clusters?
sorted_cluster_frequency <- sort(table(expedia.booked$hotel_cluster), decreasing = TRUE)
most_common_clusters <- head(as.matrix(sorted_cluster_frequency)[,0],5)



################################################
# Which columns have missing data?
# What do we wanna do?  Do we wanna make new column, like "Child" in titanic, to help trees?


# Split the data back into Kaggle's train and test sets
expedia.predict <- all_data[1:2528243,]
#expedia.data <- all_data[2528244:40198536,]  #if you didn't ignore non-bookings
expedia.booked <- all_data[2528244:5528936,]



## BUILD MODEL
## randomly choose 70% of the data set as training data
set.seed(27)
expedia.train.indices <- sample(1:nrow(expedia.booked), 0.7*nrow(expedia.booked), replace=F)
expedia.train <- expedia.booked[expedia.train.indices,]
dim(expedia.train)
summary(expedia.train$hotel_cluster)
## select the other 30% as the testing data
expedia.test <- expedia.booked[-expedia.train.indices,]
dim(expedia.test)
summary(expedia.test$hotel_cluster)
## You could also do this
#random.rows.test <- setdiff(1:nrow(expedia.data),random.rows.train)
#expedia.test <- expedia.data[random.rows.test,]

## Fit decision model to training set
expedia.rf.model <- randomForest(hotel_cluster ~ User_Location_Region + Channel + Srch_Adults_Cnt + Hotel_Country, data=expedia.train, importance=TRUE, ntree=1000, mtry=3, nodesize=5, maxnodes=200)
print(expedia.rf.model)





## MODEL EVALUATION
## Predict test set outcomes, reporting class labels
expedia.rf.predictions <- predict(expedia.rf.model, expedia.test, type="response")
## calculate the confusion matrix
expedia.rf.confusion <- table(expedia.rf.predictions, expedia.test$hotel_cluster)
print(expedia.rf.confusion)
## accuracy
expedia.rf.accuracy <- sum(diag(expedia.rf.confusion)) / sum(expedia.rf.confusion)
print(expedia.rf.accuracy)
## precision
expedia.rf.precision <- expedia.rf.confusion[2,2] / sum(expedia.rf.confusion[2,])
print(expedia.rf.precision)
## recall
expedia.rf.recall <- expedia.rf.confusion[2,2] / sum(expedia.rf.confusion[,2])
print(expedia.rf.recall)
## F1 score
expedia.rf.F1 <- 2 * expedia.rf.precision * expedia.rf.recall / (expedia.rf.precision + expedia.rf.recall)
print(expedia.rf.F1)
# We can also report probabilities
expedia.rf.predictions.prob <- predict(expedia.rf.model, expedia.test, type="prob")
print(head(expedia.rf.predictions.prob))
print(head(expedia.test))

## show variable importance
importance(expedia.rf.model)
varImpPlot(expedia.rf.model)

# Does anyone want to cross validate?