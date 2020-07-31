#  Commonality analysis for Canonical correlation using yhat package
#  The function canonCommonality returns commonality data for both canonical variable sets.

install.packages("yhat")
library(yhat)

#  ?yhat   # lists the arguments for yhat 
#  out = canonCommonality(A, B, nofns = 1)

# Uses the R builtin cancor and yacca cca example

data(LifeCycleSavings)

pop <- LifeCycleSavings[, 2:3]
oec <- LifeCycleSavings[, -(2:3)]
 
# Perform Commonality Coefficient Analysis

canonCommonData<-canonCommonality(pop,oec,1)
canonCommonData