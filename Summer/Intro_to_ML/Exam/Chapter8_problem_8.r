library (MASS)
library (ISLR)
library(tidyverse)
library(caret)
library(leaps)
attach(Carseats)
install.packages('caTools')
library(caTools)

set.seed(2)
data = Carseats
sample.data = sample.split(data$Sales, SplitRatio = 0.70)
train.set = subset(data, sample.data==T)
test.set = subset(data, sample.data==F)

#Regression tree
