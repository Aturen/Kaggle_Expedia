###################################################################################
## Initial viewing of Expedia Hotel Recommender data from Kaggle
###################################################################################

## DATA EXPLORATION AND CLEANING
## load the Expedia data in R
## Be sure your working directory is set to the cloned Expedia_Kaggle base directory
expedia.data <- read.csv("data_Known.csv", header=TRUE)
expedia.predict <- read.csv("data_Predict.csv", header=TRUE)
## explore the data set
dim(expedia.data)
str(expedia.data)
summary(expedia.data)

## Clean both sets simultaneously
# Add variable hotel_cluster to the "test" dataset with 0 Values (gets overwritten later)
expedia.predict$hotel_cluster <- 0

# Combine Kaggle's "train" and "test" datasets, name all_data.  
#  (We'll split into two groups after filling in missing values and tidying up)
all_data <- rbind(expedia.data,expedia.predict)

# Which columns have missing data?
# What do we wanna do?  Do we wanna make new column, like "Child" in titanic, to help trees?


# Split the data back into Kaggle's train and test sets
expedia.data <- all_data[1:37670293,]
expedia.predict <- all_data[37670294:37923137,]



## BUILD MODEL
## randomly choose 70% of the data set as training data
set.seed(27)
expedia.train.indices <- sample(1:nrow(expedia.data), 0.7*nrow(expedia.data), replace=F)
expedia.train <- expedia.data[expedia.train.indices,]
dim(expedia.train)
summary(expedia.train$hotel_cluster)
## select the other 30% as the testing data
expedia.test <- expedia.data[-expedia.train.indices,]
dim(expedia.test)
summary(expedia.test$hotel_cluster)
## You could also do this
#random.rows.test <- setdiff(1:nrow(expedia.data),random.rows.train)
#expedia.test <- expedia.data[random.rows.test,]

## Fit decision model to training set
expedia.rf.model <- randomForest(hotel_cluster ~ ., data=expedia.train, importance=TRUE, ntree=1000, mtry=3, nodesize=5, maxnodes=200)
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