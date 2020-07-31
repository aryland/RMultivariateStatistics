# Chapter 7 
# Polytomous Discriminant Function analysis
# Use ?lda for more information
# Data set from Field, Miles, & Field, p. 720-722
# Install and Load packages

install.packages("MASS")
library(MASS)

# Create group vector, data vectors, and data frame

Group = gl(3,10,labels=c("CBT","BT","NT"))
Actions = c(5,5,4,4,5,3,7,6,6,4,4,4,1,1,4,6,5,5,2,5,4,5,5,4,6,4,7,4,6,5)
Thoughts = c(14,11,16,13,12,14,12,15,16,11,14,15,13,14,15,19,13,18,14,17,13,15,14,14,13,20,13,16,14,18)
OCD = data.frame(Group, Actions, Thoughts)
attach(OCD)
OCD

# Run Box M test
# Install and load package

install.packages("biotools")
library(biotools)
options(scipen=999)

factor(OCD[,1]) 
boxM(OCD[2:3],OCD[,1])


# Run Discriminant analysis

fit  = lda(Group ~ Actions + Thoughts, data = OCD, prior = c(1,1,1)/3,na.action="na.omit")
fit

# Show group prediction and put in data frame then view

result = predict(fit)$class
result = cbind(result)
prior = cbind (Group)
out = data.frame(prior,result)
out

# Classification Summary
# Assess the accuracy of the prediction

ct = table(prior, result)
diag(prop.table(ct))
# total percent correct
sum(diag(prop.table(ct)))

# Show cell counts and proportions

mytable = table(Group,result)
mytable
prop.table(mytable)

# Chi-square Test of Correct Classification

chisq.test(Group,result)

# Effect Size 
# OCD groups must be numeric, so create numeric group vector

GRP=cbind(c(rep(1,10),rep(2,10),rep(3,10)))

# Install and load CCA package
# Canonical r and Bartlett chi-square in CCA package

install.packages("CCA")
library(CCA)
outfit = cca(OCD[2:3],GRP)
summary(outfit)

