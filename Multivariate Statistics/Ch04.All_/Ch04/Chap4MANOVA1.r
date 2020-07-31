# Chapter 4 MANOVA
# Field, Andy; Miles, Jeremy; Field, Zoe (2012).  Discovering Statistics Using R
# OCD data p. 721

# Install Packages

install.packages("MASS")      # MANOVA
install.packages("car")       # Type III SS
install.packages("pastecs")   # descriptive statistics

# Load packages

library(MASS); library(car); library(pastecs)

# Create group vector and data vectors

Group = gl(3,10,labels=c("CBT","BT","NT"))
Actions = c(5,5,4,4,5,3,7,6,6,4,4,4,1,1,4,6,5,5,2,5,4,5,5,4,6,4,7,4,6,5)
Thoughts = c(14,11,16,13,12,14,12,15,16,11,14,15,13,14,15,19,13,18,14,17,13,15,14,14,13,20,13,16,14,18)

# Put in a Data Set and View

OCD = data.frame(Group, Actions, Thoughts)
OCD

# Descriptive statistics by dependent variable

by(OCD$Actions, OCD$Group, stat.desc, basic=FALSE)
by(OCD$Thoughts, OCD$Group, stat.desc, basic=FALSE)

# MANOVA

outcome = cbind(OCD$Actions,OCD$Thoughts)
model = manova(outcome ~ Group, data = OCD)

# MANOVA output


options(scipen=999)
summary(model,intercept = TRUE)
summary(model,intercept = TRUE, test = 'Wilks')
summary(model,intercept = TRUE, test = 'Hotelling')
summary(model,intercept = TRUE, test = 'Roy')

# What about assumptions - check for outliers and normality
# What about interpretation - need univariate tests

