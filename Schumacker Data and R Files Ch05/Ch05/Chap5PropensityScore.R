# Chapter 5 MANCOVA
# Propensity score matching - nearest neighbor
# Requires R 3.2.2 version or higher

install.packages("MatchIt")
install.packages("Hmisc")
library(MatchIt)
library(Hmisc)

# Read in SPSS data set in R with value labels -  last option converts value labels to R factors 

mydata = spss.get("C:/propensity.por", use.value.labels=FALSE)
mydata

# Missing values corrected in SPSS file  or  finaldata = as.data.frame(na.omit(mydata)) 
# Matching is performed using propensity scores with covariate variables 
# Data set must not have missing values and Y variable must be 0 or 1 coded

m.out = matchit(STATUS~GENDER+RACE+GRADUATE, method="nearest", data=mydata, + ratio = 1)
m.out 

# Final matched data saved as final.data

final.data = match.data(m.out)
final.data

# Set directory to save file

setwd("C:/")

# Write out the 279 AP students that match the IB students on covariates 

write.table(final.data,file="match1",sep=" ", row.names=TRUE, col.names = TRUE,quote=FALSE)

##  NOTE - These results were used to select AP students listed in full data set
##  Full data set of N = 279 IB and N = 279 AP not given 




