library (MASS)

summary(Boston)

attach(Boston)

#--------------------------------------------------Part (a)----------------------------------------------------------------------------#

#statistical significance is denoted by the p-value. P value disproves the null hypothesis, that means for a p value higher than 0.05, 
#it indicates that the variable does not have statistical significance

lm.zn = lm(crim~zn)
summary(lm.zn) # statistically significant
lm.indus = lm(crim~indus)
summary(lm.indus) # statistically significant
lm.chas = lm(crim~chas) 
summary(lm.chas) # Very high p-value,0.2, which is greater than 0.05. 
lm.nox = lm(crim~nox)
summary(lm.nox) # statistically significant
lm.rm = lm(crim~rm)
summary(lm.rm) # statistically significant
lm.age = lm(crim~age)
summary(lm.age) # statistically significant
lm.dis = lm(crim~dis)
summary(lm.dis) # statistically significant
lm.rad = lm(crim~rad)
summary(lm.rad) # statistically significant
lm.tax = lm(crim~tax)
summary(lm.tax) # statistically significant
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # statistically significant
lm.black = lm(crim~black)
summary(lm.black) # statistically significant
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # statistically significant
lm.medv = lm(crim~medv)
summary(lm.medv) # statistically significant

#plot(lm.chas)
#plot(lm.zn)
#plot(lm.nox)
#plot(lm.rm)
#plot(lm.dis)
#plot(lm.rad)
#plot(lm.tax)
#plot(lm.ptratio)
#plot(lm.lstat)
#plot(lm.medv)

#--------------------------------------------------Part (b)----------------------------------------------------------------------------#

lm.multi= lm(crim~., data=Boston)
summary(lm.multi)
plot(lm.multi)

#zn, dis, rad, black and medv have Pr(>|t|) values less than 0.05, hence we can reject null hypothesis for these predictors

#--------------------------------------------------Part (c)----------------------------------------------------------------------------#
#how do your results from a compare to b? One is uni variate regression and the other is Multivariate regression. 
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.multi)[2:14]
plot(x, y)

#the coefficient estimate of nox is -10 in multivariate regression and 31 in uni variate regression. If we remove nox from set of variables in multivariate analysis, the effect of other variables would be the same for their respective univariate regressions. 


#--------------------------------------------------Part (d)----------------------------------------------------------------------------#


lm.zn2 = lm(crim~poly(zn,3))
summary(lm.zn2) 
#Pr(>|t|) value of  coeff of cubic variable is greater than 0.05, therefore this is not statistically significant ,hence no non-linear effect

lm.indus2 = lm(crim~poly(indus,3))
summary(lm.indus2)
#all Pr(>|t|) value of all coefficients is below 0.05, this indicates adequacy of cubic fit,therefore non-linear relationship is observed


#lm.chas2 = lm(crim~poly(chas,3))
#summary(lm.chas2)
#lm of chas gives error because this is a qualitative predictor, as it only has 2 values 0 and 1. The error states the degree must be less than number of unique points

lm.nox2 = lm(crim~poly(nox,3))
summary(lm.nox2)
#all Pr(>|t|) value of all coefficients is below 0.05, this indicates adequacy of cubic fit,therefore non-linear relationship is observed



lm.rm2 = lm(crim~poly(rm,3))
summary(lm.rm2)
#Pr(>|t|) value of  coeff of cubic variable is greater than 0.05, therefore this is not statistically significant,hence no non-linear effect


lm.age2 = lm(crim~poly(age,3))
summary(lm.age2)
#all Pr(>|t|) value of all coefficients is below 0.05, this indicates adequacy of cubic fit,therefore non-linear relationship is observed


lm.dis2 = lm(crim~poly(dis,3))
summary(lm.dis2) 
#all Pr(>|t|) value of all coefficients is below 0.05, this indicates adequacy of cubic fit, therefore non-linear relationship is observed


lm.rad2 = lm(crim~poly(rad,3))
summary(lm.rad2) 
#Pr(>|t|) value of  coeff of cubic variable is greater than 0.05, therefore this is not statistically significant,hence no non-linear effect


lm.tax2 = lm(crim~poly(tax,3))
summary(lm.tax2)
#Pr(>|t|) value of  coeff of cubic variable is greater than 0.05, therefore this is not statistically significant ,hence no non-linear effect

lm.ptratio2 = lm(crim~poly(ptratio,3))
summary(lm.ptratio2)
#all Pr(>|t|) value of all coefficients is below 0.05, this indicates adequacy of cubic fit, therefore non-linear relationship is observed


lm.black2 = lm(crim~poly(black,3))
summary(lm.black2)
#2 Pr(>|t|) values of quadratic and cubic coefficients is above 0.05, therefore no linear relationship is observed

lm.lstat2 = lm(crim~poly(lstat,3))
summary(lm.lstat2)
#Pr(>|t|) value of  coeff of cubic variable is greater than 0.05, therefore this is not statistically significant ,hence no non-linear effect



lm.medv2 = lm(crim~poly(medv,3))
summary(lm.medv2)
#all Pr(>|t|) value of all coefficients is below 0.05, this indicates adequacy of cubic fit, therefore non-linear relationship is observed

