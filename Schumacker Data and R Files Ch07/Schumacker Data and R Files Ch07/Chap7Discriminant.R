#  Chapter 7 Discriminant Analysis
#  Use ?lda for more information

# Install and Load packages

install.packages("MASS")
library(MASS)

# Create group vector and data vectors

Group = gl(3,10,labels=c("CBT","BT","NT"))
Actions = c(5,5,4,4,5,3,7,6,6,4,4,4,1,1,4,6,5,5,2,5,4,5,5,4,6,4,7,4,6,5)
Thoughts = c(14,11,16,13,12,14,12,15,16,11,14,15,13,14,15,19,13,18,14,17,13,15,14,14,13,20,13,16,14,18)

# Put in a Data Set and View

OCD = data.frame(Group, Actions, Thoughts)
OCD

# Use lda function to Run discriminant Analysis

fit  = lda(Group ~ Actions + Thoughts, data = OCD, prior = c(1,1,1)/3,na.action="na.omit")
fit

# Show group prediction and put in data frame then view

result = predict(fit)$class
result = cbind(result)
prior = cbind (Group)
out = data.frame(prior,result)
out

# Assess the accuracy of the prediction

ct <- table(prior, result)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# Show cell counts and proportions

mytable = table(Group,result)
mytable

prop.table(mytable)

# Chi-square Test of Correct Classification

CrossTable(out$Group,out$result, chisq=TRUE, fisher=TRUE)


