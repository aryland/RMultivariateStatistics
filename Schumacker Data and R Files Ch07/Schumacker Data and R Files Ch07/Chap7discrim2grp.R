#  Chapter 7 Two Group Discriminant Analysis
#  Use ?lda for more information

#  Install and load package

install.packages("MASS")
library(MASS)

# Create data set

group = gl(2,6,labels=c("at-risk","not at-risk"))
math = c(2,3,5,2,4,6,5,7,8,10,9,7)
english=c(3,4,4,5,8,7,6,6,7,8,5,6)

discrim = data.frame(group, math, english)
attach(discrim)

# Run two group discriminant function analysis 

out = lda(group~ math + english, data = discrim)
out

#########################
Box M test
#########################

install.packages("biotools")
library(biotools)
options(scipen=999)

factor(discrim[,1])
boxM(discrim[2:3],discrim[,1])

###############################
# Classification summary table
#################################

# Create data with group and predicted values

result = predict(out)$class
result = cbind(result)
prior = cbind (group)
final = data.frame(prior,result)
final

#  Values in the diagonal of summary table  

ct <- table(prior, result)
diag(prop.table(ct,))

# percent correctly classified into two groups 

sum(diag(prop.table(ct)))

# Show cell counts and proportions

mytable = table(group,result)
mytable
prop.table(mytable)

############################################
# Chi-square Test of Correct Classification
############################################

attach(final)  # Permits using variable names in R commands
chisq.test(group,result,correct=FALSE)  # No Yates correction for cell size < 5

###############################################
# Optional chi-square with classification table
##################################################

install.packages("gmodels")
library(gmodels)

CrossTable(final$group,final$result, chisq=TRUE)


