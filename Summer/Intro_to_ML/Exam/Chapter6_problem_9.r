
#install.packages('ISLR')
#install.packages('glmnet')
#install.packages('pcr')
#install.packages('pls')
library(ISLR)
library(glmnet)
library(pcr)
library(pls)


set.seed(2)

College.data <- College

x = model.matrix(Apps~.,College)[,-1]
y = College$Apps

#---------------------------------------------------------Part a---------------------------------------------------------#
# Training and test sets
college_train = sample(1:nrow(x), nrow(x)/1.2)
college_test = (-college_train)
y.test = y[college_test]


#---------------------------------------------------------Part b---------------------------------------------------------#

# Linear model using least squares (that is ridge regression with lambda=0) and get the test MSE
grid = 10^seq(10,-2,length=100)
#fit
linear.model.leastsq = glmnet(x[college_train,], y[college_train], alpha=0, lambda=grid)
#predict
linear.pred = predict(linear.model.leastsq, s=0, newx=x[college_test,],exact=T,x=x[college_train,],y=y[college_train])

#test out the mean sq error
mean.lm=mean((linear.pred-y.test)^2)
mean.lm
#---------------------------------------------------------Part c---------------------------------------------------------#

#ridge regression model with lambda from cross validation

cv.out.ridge = cv.glmnet(x[college_train,],y[college_train],alpha=0)
best_lambda_ridge = cv.out.ridge$lambda.min
best_lambda_ridge

ridge.regression = glmnet(x[college_train,],y[college_train],alpha=0,lambda=grid)
ridge.pred = predict(ridge.regression, s=best_lambda_ridge, newx=x[college_test,])
mean.ridge = mean((ridge.pred-y.test)^2)
mean.ridge


#---------------------------------------------------------Part d---------------------------------------------------------#
#My lasso is doing bad
cv.out.lasso =  cv.glmnet(x[college_train,],y[college_train], alpha=1, lambda=grid)
best_lambda_lasso =  cv.out.lasso$lambda.min
best_lambda_lasso

lasso.regression = glmnet(x[college_train,],y[college_train],alpha=0,lambda=grid)
lasso.pred = predict(lasso.regression, s=best_lambda_lasso, newx=x[college_test,])
mean.lasso = mean((lasso.pred-y.test)^2)
mean.lasso


#lasso coefficients
lasso.coef = predict(lasso.regression, type="coefficients", s=best_lambda_lasso)
length(lasso.coef)
#all 18 coefficients of the lasso regression are non-zero


#---------------------------------------------------------Part e---------------------------------------------------------#
#PCR also doing bad

#PCR model on the training set, with M chosen by cross validation

pcr.fit = pcr(College$Apps~.,data=College, subset=college_train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
#best MSEP observed for components greater than 15
summary(pcr.fit)

pcr.pred = predict(pcr.fit,x[college_test,], ncomp = 17)
mean.pcr = mean((pcr.pred-y.test)^2)
mean.pcr
#selected value of M is 17 for which MSE is lowest




#---------------------------------------------------------Part f---------------------------------------------------------#


pls.fit = plsr(College$Apps~., data=College, subset=college_train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")

pls.pred = predict(pls.fit, x[college_test,], ncomp=8)
#pls.pred = predict(pls.fit, x[college_test,], ncomp=9)
#pls.pred = predict(pls.fit, x[college_test,], ncomp=7)
#pls.pred = predict(pls.fit, x[college_test,], ncomp=10)


mean.pls = mean((pls.pred-y.test)^2)
mean.pls

#lowest MSE observed for M=8

#---------------------------------------------------------Part g---------------------------------------------------------#

all_means = c(mean.lm, mean.ridge, mean.lasso, mean.pcr, mean.pls)
barplot(all_means, xlab="Models", ylab="Test MSE", names=c("lm", "ridge", "lasso", "pcr", "pls"))

#pls found to have lowest MSE relatively.

#MSEs changes if we change the seed set initially




test.avg = mean(y.test)
r2.lm = 1 - mean((linear.pred - y.test)^2) / mean((test.avg - y.test)^2)
r2.ridge = 1 - mean((ridge.pred - y.test)^2) / mean((test.avg - y.test)^2)
r2.lasso = 1 - mean((lasso.pred - y.test)^2) / mean((test.avg - y.test)^2)
r2.pcr = 1 - mean((pcr.pred - y.test)^2) / mean((test.avg - y.test)^2)
r2.pls = 1 - mean((pls.pred - y.test)^2) / mean((test.avg - y.test)^2)


all_r2 = c(r2.lm, r2.ridge, r2.lasso, r2.pcr, r2.pls)
xx <- barplot(all_r2, xlab="Models", ylab="R Square", names=c("lm", "ridge", "lasso", "pcr", "pls"))
text(x = xx, y = all_r2, label = all_r2, cex = 0.8, col = "red")


#almost all models have r2 slightly below or above 0.85. Ridge and PLS have a R2 above 0.85 accurately and others have a R2 very slightly below 0.85
#SO we can be reasonably confident about the accuracy of the predictions of Ridge and PLS.

