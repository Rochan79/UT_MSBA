# Responsible for doing kNN of sales and boosting
#install.packages('tidyverse')
#install.packages('lubridate')
library(tidyverse)
library(lubridate)

# read the data
walmart <- read.csv('Cleaned walmart data.csv')
attach(walmart)

# remove 'Date' column 
walmart <- walmart %>% select(-Date)
walmart <- walmart %>% select(-X)

# graphical representation of sales over the years
sales_per_week <- ts(walmart$Weekly_Sales, start=2010, end=2012, frequency = 142)
plot(sales_per_week)

# create training set and test set
library(tree)
set.seed(1)
# remove 'Store' column temporarily
#walmart <- walmart %>% select(-Store)
train = sample(nrow(walmart)*0.8)
# training set
w.train = walmart[train,]
# test set
w.test = walmart[-train,]

#------- DO MULTIPLE LINERAR REGRESSION ON SALES -------
lm.fit <- lm(Weekly_Sales~., w.train)
#plot(lm.fit) # pattern in the Normal Q-Q means not linear
summary(lm.fit)

# calculate test MSE
yhat = predict(lm.fit, w.test)
mean((yhat-w.test$Weekly_Sales)^2)
# the MSE is 215285754095; RMSE is 463989

# calculate accuracy
# the summary table said 0.104 is r^2
# that means this model has a 10.4% accuracy?


#------- DO MODEL SELECTION TO FIND PREDICTORS ----
library(glmnet)
logWeeklySales = log(walmart$Weekly_Sales)
w <- walmart %>% select(-Weekly_Sales)

n = dim(walmart)[1]
tr = sample(1:n,5000)

# create full matrix of interactions
XXw = model.matrix(~.*Store*Unemployment, data = data.frame(scale(w)))[,-1]
Wdata = data.frame(logWeeklySales, XXw)

null = lm(logWeeklySales~1, data=Wdata[tr,])
full = lm(logWeeklySales~., data=Wdata[tr,])

regForward = step(null, scope=formula(full), direction="forward", k=log(length(tr)))

summary(lm(logWeeklySales~., data=Wdata[tr,]))

#------- PERFORM BOOSTING ON SALES -------
# fit a boosting model to training set and find important predictors
# use 1,000 trees and shrinkage value of 0.01
library(gbm)
set.seed(2)

# fit boosting model
boost.w = gbm(Weekly_Sales~., data=w.train, distribution="gaussian", n.trees=1000, shrinkage=0.01)
# find important predictors and plot
summary(boost.w)# the store far exceeds any other predictor; if removed it becomes unemployment

# predict weekly sales based on boosted model
yhat.boost = predict(boost.w, w.test, n.trees=1000)
mean((yhat.boost-w.test$Weekly_Sales)^2)
# find test MSE
# the test MSE is 249535726390; RMSE is 499535.5
# find the accuracy
# the accuracy or r^2 was 


#Bagging
library(randomForest)
set.seed(100)
bag.w = randomForest(Weekly_Sales~., data=w.train,mtry = 9, importance = TRUE)

#mse of random forest
yhat.bag = predict(bag.w, w.test, n.trees=1000)
mean((yhat.bag-w.test$Weekly_Sales)^2)

#using importance and plotting to analyze
importance(bag.w)
varImpPlot(bag.w)


#using randomforest to find new mse
#lowest MSE is mtry = 5 so to avoid overfitting, using that.

#rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 1, importance = TRUE)
#yh.rf = predict(rf.w,newdata=w.test)
#mean((yh.rf-w.test$Weekly_Sales)^2)
#importance(rf.w)

#rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 2, importance = TRUE)
#yh.rf = predict(rf.w,newdata=w.test)
#mean((yh.rf-w.test$Weekly_Sales)^2)
#importance(rf.w)

#rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 3, importance = TRUE)
#yh.rf = predict(rf.w,newdata=w.test)
#mean((yh.rf-w.test$Weekly_Sales)^2)
#importance(rf.w)

#rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 4, importance = TRUE)
#yh.rf = predict(rf.w,newdata=w.test)
#mean((yh.rf-w.test$Weekly_Sales)^2)
#importance(rf.w)

rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 5, importance = TRUE)
yh.rf = predict(rf.w,newdata=w.test)
mean((yh.rf-w.test$Weekly_Sales)^2)
importance(rf.w)
varImpPlot(rf.w)

#rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 6, importance = TRUE)
#yh.rf = predict(rf.w,newdata=w.test)
#mean((yh.rf-w.test$Weekly_Sales)^2)
#importance(rf.w)

#rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 7, importance = TRUE)
#yh.rf = predict(rf.w,newdata=w.test)
#mean((yh.rf-w.test$Weekly_Sales)^2)
#importance(rf.w)

#rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 8, importance = TRUE)
#yh.rf = predict(rf.w,newdata=w.test)
#mean((yh.rf-w.test$Weekly_Sales)^2)
#importance(rf.w)

#rf.w = randomForest(Weekly_Sales~., data=w.train,mtry = 9, importance = TRUE)
#yh.rf = predict(rf.w,newdata=w.test)
#mean((yh.rf-w.test$Weekly_Sales)^2)
#importance(rf.w)