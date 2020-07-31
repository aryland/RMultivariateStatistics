# Chapter 7 Discriminant Analysis
# Exercise 3 Answers

# Lists data sets in base package
# data()

# Use data set amis in boot package

install.packages("boot")
library(boot)

data(amis)
attach(amis)
head(amis,10)

# Conduct discriminant analysis

install.packages("MASS")
library(MASS)

myout = lda(period ~ speed + warning)
myout

# Output actual and predicted values

myresult = predict(myout)$class
myresult = cbind(myresult)
myprior = cbind (period)
myout = data.frame(myprior,myresult)
head(myout,10)

# Assess the accuracy of the prediction
# total percent correct

count = table(myprior, myresult)
diag(prop.table(count))
sum(diag(prop.table(count)))


# Show cell counts and proportions

mytable = table(myprior,myresult)
mytable
prop.table(mytable)

# Compute chi-square
options(scipen=999)
chisq.test(myprior,myresult)

# Effect Size
# Groups must be numeric
# Install and load package
# Canonical r and Bartlett chi-square in package

install.packages("yacca")
library(yacca)

DV = cbind(speed,warning)
outfit = cca(DV,period)
summary(outfit)

