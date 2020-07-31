# Chapter 12 SEM Basic Model
# Tabachnick & Fidell, 5th Edition (2007) p. 686
# Covariance matrix of 5 continuous variables, n = 100 skiers
# NUMYRS:  number of years skiiing
# DAYSKI:  number of days skiing
# SNOWSAT: satisfaction with snow conditions
# FOODSAT: satisfaction with the food
# SENSEEK: degree of sensation seeking


library(lavaan)

# Input correlation matrix

ski = data.frame(lower2full(c(1,.7,11.47,.62,.62,1.87,.44,.44,.95,1.17,.3,.21,.54,.38,1),diagonal=TRUE),row.names=c("NUMYRS","DAYSKI","SNOWSAT","FOODSAT","SENSEEK"))
names(ski) = c("NUMYRS","DAYSKI","SNOWSAT","FOODSAT","SENSEEK") 
ski = as.matrix(ski)
ski

basic.model = '
loveski =~ NUMYRS + DAYSKI
skisat =~ SNOWSAT + FOODSAT
skisat ~ SENSEEK + loveski'

fit.index = c("chisq","df","pvalue","cfi","rmsea","gfi")

basic.fit = sem(model=basic.model,sample.cov = ski,sample.nobs = 100,std.lv=TRUE)
fitMeasures(basic.fit,fit.index)
summary(basic.fit,standardized=TRUE)

# Graph SEM model

library(psych)

# Poor graphics
#lavaan.diagram(basic.fit)
#

fx = matrix(c(.81,.86),ncol=1)
fy = matrix(c(1.1,.78),ncol=1)
Phi = matrix(c(1,0,.56,0,1,0),ncol=2,byrow=TRUE)
f1= structure.diagram(fx,Phi,fy)



############################
#  example in R
###########################

fx <- matrix(c(.9,.8,.6,rep(0,4),.6,.8,-.7),ncol=2)
fy <- matrix(c(.6,.5,.4),ncol=1)
Phi <- matrix(c(1,0,0,0,1,0,.7,.7,1),ncol=3,byrow=TRUE)
f1 <- structure.diagram(fx,Phi,fy,main="A structural path diagram")
f1