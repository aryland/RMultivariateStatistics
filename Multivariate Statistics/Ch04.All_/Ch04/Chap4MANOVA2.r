# Chapter 4 MANOVA

# Field, Andy; Miles, Jeremy; Field, Zoe (2012).  Discovering Statistics Using R
# OCD data p. 721
# Check assumptions and univariate for interpretation

# Install Packages
install.packages("MASS")      # MANOVA
install.packages("car")       # Type III SS
install.packages("mvoutlier") # outlier test
install.packages("mvnormtest")# multivariate normality
install.packages("pastecs")   # descriptive statistics
install.packages("reshape")   # reshape data
install.packages("stats")     # statistical procedures

# Load packages

library(MASS); library(car); library(mvoutlier); library(stats)
library(mvnormtest); library(pastecs); library(reshape)

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

#  Print out Group variance- covariance matrices

by(OCD[,2:3], OCD$Group,cov)
bartlett.test(OCD$Actions ~ OCD$Group, data = OCD)
bartlett.test(OCD$Thoughts ~ OCD$Group, data = OCD)

# Multivariate normality using mshapiro.test in mvnormtest package

cbt = OCD[1:10,2:3]  # select data for each group
bt = OCD[11:20,2:3]
nt = OCD[21:30,2,3]

cbt = t(cbt)         # transform data to rows in each group
bt =  t(bt)
nt =  t(nt)

mshapiro.test(cbt)   # run normality test
mshapiro.test(bt)
mshapiro.test(nt)

# Outlier Graph using mvoutlier package - case 26 an outlier

aq.plot(OCD[,2:3])   # omit group in column 1, read column 2 and 3

# MANOVA

outcome = cbind(OCD$Actions,OCD$Thoughts)
model = manova(outcome ~ Group, data = OCD)

# MANOVA output

summary(model,intercept = TRUE)
summary(model,intercept = TRUE, test = 'Wilks')
summary(model,intercept = TRUE, test = 'Hotelling')
summary(model,intercept = TRUE, test = 'Roy')


# Type I, II, and III SS the same because of 1 predictor (Group)
# In balanced designs and single predictor they are the same SS
# In two or more predictors they are different, Type I depends on variable order
# Type II and III do not depend on variable order

model1 = aov(OCD$Actions ~ Group)
Anova(model1, type = "II")
Anova(model1, type = "III")

model2 = aov(OCD$Thoughts ~ Group)
Anova(model2, type = "II")
Anova(model2, type = "III")


#  Type of therapy employed had a significant effect on OCD in multivariate
#  Nature of effect not clear from multivariate test
#  Run univariate test to interpret results
# Individual ANOVA shows no statistical significance of Action and Thoughts


# Correlation of dependent variables gave multivariate an edge

cor.test(OCD$Actions,OCD$Thoughts)
 
