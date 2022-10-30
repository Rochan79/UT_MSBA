# #10. This exercise involves the Boston housing data set.
# (a) To begin, load in the Boston data set. The Boston data set is
# part of the MASS library in R.
# > library (MASS)
# Now the data set is contained in the object Boston.
# > Boston
# Read about the data set:
#   > ?Boston
# How many rows are in this data set? How many columns? What
# do the rows and columns represent?
#   (b) Make some pairwise scatterplots of the predictors (columns) in
# this data set. Describe your findings.
# (c) Are any of the predictors associated with per capita crime rate?
#   If so, explain the relationship.
# (d) Do any of the suburbs of Boston appear to have particularly
# high crime rates? Tax rates? Pupil-teacher ratios? Comment on
# the range of each predictor.
# (e) How many of the suburbs in this data set bound the Charles
# river?
# (f) What is the median pupil-teacher ratio among the towns in this
# data set?
#   (g) Which suburb of Boston has lowest median value of owneroccupied
# homes? What are the values of the other predictors
# for that suburb, and how do those values compare to the overall
# ranges for those predictors? Comment on your findings.
# (h) In this data set, how many of the suburbs average more than
# seven rooms per dwelling? More than eight rooms per dwelling?
#   Comment on the suburbs that average more than eight rooms
# per dwelling.
library (MASS)
# ? Boston
summary(Boston)
#rows_indata=nrow(Boston)
#print('Number of rows in Boston: ')
#print(rows_indata)
#-----------------------------------------------------Part A---------------------------------------------------------------#
?Boston #help shows number of rows and columns and their description
dim(Boston) #output shows 506 rows and 14 columns

#---------------------------------------------------Part b------------------------------------------------------------------------------#
# Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings
#pairs(Boston)  uninterpretable
pairs(~crim++zn+indus+tax+black+nox+dis+tax+medv, data=Boston)


#observations-
#crim seems to have a negative trend with dis and medv as we can plot a negative exponential kind of curve scatter plot
#nox seems to have a negative trend with dis as we can plot a negative exponential kind of curve in their scatter plot
#dis also seems to have a negative trend with nox and a positive trend with medv 
#unable to see a trend with tax
#medv has a positive trend with dis and negative trend with nox
#medv has a positive trend with black
#indus and a negative trend with dis and medv
#zn has a positive trend with dis and medv maybe

#---------------------------------------------------Part c------------------------------------------------------------------------------#
#to find predictors we have to find the correlation coefficient
cor(Boston$crim,Boston$zn)
cor(Boston$crim,Boston$indus)
cor(Boston$crim,Boston$chas)
cor(Boston$crim,Boston$nox)
cor(Boston$crim,Boston$rm)
cor(Boston$crim,Boston$age)
cor(Boston$crim,Boston$dis)
cor(Boston$crim,Boston$rad)
cor(Boston$crim,Boston$tax)
cor(Boston$crim,Boston$ptratio)
cor(Boston$crim,Boston$black)
cor(Boston$crim,Boston$lstat)
cor(Boston$crim,Boston$medv)
# criM has a negative  relationship with zn, rm,  medv, dis, black.
#crim has a positive  relationship with indus, nox, rad, tax, ptratio, lstat, age.

#---------------------------------------------------Part d------------------------------------------------------------------------------#
#High crime rate= crime rate >95% of suburbs which is 2 std dev from the mean

nrow(Boston[which(Boston$crim > mean(Boston$crim) + 2*sd(Boston$crim)),])
#there are 16 suburbs with particularly high crime rates
range(Boston$crim)
#Crime ranges from as close to 0 to 89, a very wide range         
mean(Boston$crim)
#mean crime rate is 3.613
sd(Boston$crim)
#std dev of crime rate is 8.6, that means some suburbs have extremely high crime rates as the range goes upto 89!


nrow(Boston[which(Boston$tax > mean(Boston$tax) + 2*sd(Boston$tax)),])
#there are 0 suburbs with particularly high tax rates
nrow(Boston[which(Boston$tax > mean(Boston$tax) + sd(Boston$tax)),])
#there are 137 suburbs with tax rates higher than 1 std dev
range(Boston$tax)
#tax ranges from as close to 187 to 711, this is also a very wide range        
mean(Boston$tax)
#mean tax rate is 408.23
sd(Boston$tax)
#std dev of tax rate is 168, that means some suburbs have extremely high tax rates.





nrow(Boston[which(Boston$ptratio > mean(Boston$ptratio) + 2*sd(Boston$ptratio)),])
#there are 0 suburbs with particularly high ptratio
nrow(Boston[which(Boston$ptratio > mean(Boston$ptratio) + sd(Boston$ptratio)),])
#56 suburbs have mean ptratio> 1 std dev.  
range(Boston$ptratio)
#ptratio ranges from as close to 12.6 to 22, the range is very narrow     
mean(Boston$ptratio)
#mean ptratio rate is 18
sd(Boston$ptratio)
#std dev of ptratio rate is 2.16


#---------------------------------------------------Part e------------------------------------------------------------------------------#

sum(Boston$chas==1)
#35 suburbs bound the Charles river

#---------------------------------------------------Part f------------------------------------------------------------------------------#


median(Boston$ptratio)
#the median pupil to teacher ratio is 19.05

#---------------------------------------------------Part g------------------------------------------------------------------------------#

#which(Boston$age == min(Boston$age))
which(Boston$medv == min(Boston$medv))

#there are 2 suburbs 399 and 406 that have the lowest median property values

range(Boston$crim) 
range(Boston$zn) 
range(Boston$indus) 
range(Boston$chas) 
range(Boston$nox) 
range(Boston$rm) 
range(Boston$age) 
range(Boston$dis) # 
range(Boston$rad) # 
range(Boston$tax) #
range(Boston$ptratio) #
range(Boston$lstat) #
range(Boston$medv) #

mean(Boston$crim)
mean(Boston$zn)
mean(Boston$indus)
mean(Boston$chas)
mean(Boston$nox)
mean(Boston$rm)
mean(Boston$age)
mean(Boston$dis)
mean(Boston$rad)
mean(Boston$tax)
mean(Boston$ptratio)
mean(Boston$lstat)
mean(Boston$medv)


sd(Boston$crim)
sd(Boston$zn)
sd(Boston$indus)
sd(Boston$chas)
sd(Boston$nox)
sd(Boston$rm)
sd(Boston$age)
sd(Boston$dis)
sd(Boston$rad)
sd(Boston$tax)
sd(Boston$ptratio)
sd(Boston$lstat)
sd(Boston$medv)

# Values of other predictors for suburb 399
Boston[399,]
#high crime rate 
#lowest zn
#proportion of non-retail business acres per tow is average  
#not bound to charles river
#nox very close to average
#a little less than mean rm
#proportion of owner-occupied units built prior to 1940 higher than mean age
#higher lstat(percentage of lower status of population) than average of all
#lowest median value of owner-occupied homes in $1000s.


# Values of other predictors for suburb 406
Boston[406,]
#crim rate higher than the other suburb.
#lowest zn
#proportion of non-retail business acres per tow is average  
#not bound to charles river
#nox very close to average
#a little less than mean rm
#proportion of owner-occupied units built prior to 1940 higher than mean age
#higher lstat(percentage of lower status of population) than average of all
#lowest median value of owner-occupied homes in $1000

#parameters for both these suburbs are more or less similar


#---------------------------------------------------Part h------------------------------------------------------------------------------#

length(which(Boston$rm > 7))
#there are 64 suburbs with average more than 7 number of rooms per dwelling.

length(which(Boston$rm > 8))
#there are 13 suburbs with average more than 7 number of rooms per dwelling.

summary(Boston)
summary(subset(Boston,Boston$rm > 8))
#very less crime rate crim, lstat and higher medv than average.
