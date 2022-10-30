library (MASS)
library (ISLR)
summary(Boston)
library(tidyverse)
library(caret)
library(leaps)
attach(Boston)

#install.packages('leaps')
library (regsubsets)
#install.packages('caret')



#-----------------------------------------------------Part a----------------------------------------------------------------------------#
#Best subset selection
# Best subset selection using cross validation with 10 folds.

predict.regsubsets <- function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}
set.seed(121)
k = 10
folds = sample(1:k, nrow(Boston), replace=TRUE)
cv.errors = matrix(NA,k,13, dimnames = list(NULL, paste(1:13)))
for (j in 1:k) {
  best.fit = regsubsets(crim ~ ., data = Boston[folds != j, ], nvmax = 13)
  for (i in 1:13) {
    pred = predict(best.fit, Boston[folds == j, ], id = i)
    cv.errors[j, i] = mean((Boston$crim[folds == j] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors, 2, mean)
plot(1:13, mean.cv.errors, xlab = "Number of variables", ylab = "CV error",
     main= "Best subset selection", pch = 1, type = "b")

#CV error • CV error is lowest for model with 9 variables. CV Error = 42.01511


#Lasso Model 
set.seed(2)
x = model.matrix(crim~.,Boston)[,-1]
y = Boston$crim
grid = 10^seq(10,-2,length=100)
train =  sample(1:nrow(x), nrow(x)/1.2)

test = (-train)
y.test = y[test]
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
best_lambda_lasso = cv.out$lambda.min
lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda=grid)
lasso.pred = predict(lasso.mod, s=best_lambda_lasso, newx=x[test,])


mean((lasso.pred-y.test)^2)



lasso.coef = predict(lasso.mod, type="coefficients", s=best_lambda_lasso)[1:13,]
lasso.coef


#Ridge Regression.

cv.out = cv.glmnet(x[train,], y[train], alpha=0)
best_lambda_ridge = cv.out$lambda.min
glm.mod = glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
glm.pred = predict(glm.mod, s=best_lambda_ridge, newx=x[test,])
mean((glm.pred-y.test)^2)


glm.coef = predict(glm.mod, type="coefficients", s=best_lambda_ridge)[1:13,]
glm.coef


#PCR
pcr.fit = pcr(crim~., data=Boston, subset=train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred = predict(pcr.fit, x[test,], ncomp=13)
mean((pcr.pred-y.test)^2)



#I would choose the Lasso model because it gives me the least MSE compared to other models
#Lasso models are generally more interpret able.
#It results in a sparse model with 10 variables. Two variables whose effect on the response were below the required threshold were removed.
