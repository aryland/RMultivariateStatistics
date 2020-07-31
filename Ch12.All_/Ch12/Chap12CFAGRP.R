# Chapter 12 Multiple Group Analysis
# Raykov & Marcoluides (2008, p. 317)
# data files ch9ex4; ch9ex4-boys; ch9ex4-girls

library(lavaan)

# Read in data sets and create full covariance matrices

girls = lower2full(c(
112.35,
77.22,103.42,
69.33,74.44,109.16,
21.22,23.46,29.51,111.32,
20.35,28.23,31.43,78.54,104.49,
21.53,34.33,29.23,72.75,87.97,114.62))

boys = lower2full(c(
113.38,
78.23,105.49,
69.92,74.43,108.15,
22.25,22.45,29.71,112.38,
21.32,27.26,31.63,78.65,105.49,
22.53,33.37,29.43,72.95,88.43,113.60))

#  Assign variable names and define as matrix for computations

girls = data.frame(girls,row.names=c("RA1","RA2","RA3","MA1","MA2","MA3"))
names(girls) = c("RA1","RA2","RA3","MA1","MA2","MA3")
girls = as.matrix(girls)

girls  # covariance matrix for girls


boys = data.frame(boys,row.names=c("RA1","RA2","RA3","MA1","MA2","MA3"))
names(boys) = c("RA1","RA2","RA3","MA1","MA2","MA3")
boys = as.matrix(boys)

boys    # covariance matrix for boys

# Create CFA bi-factor model

cfa2.model ='
  read =~RA1 + RA2 + RA3
  math =~MA1 + MA2 + MA3
  read~~math'

# Select fit indices

fit.index = c("chisq","df","pvalue","cfi","rmsea","gfi")

# Run CFA girls 

girl.fit = cfa(cfa2.model,sample.cov = girls,sample.nobs=230,std.lv=FALSE)
fitMeasures(girl.fit,fit.index)
summary(girl.fit, standardized = TRUE)


# Run CFA boys 

boy.fit = cfa(cfa2.model,sample.cov = boys,sample.nobs=215,std.lv=FALSE)
fitMeasures(boy.fit,fit.index)
summary(boy.fit, standardized = TRUE)


# Compare CFA models

anova(girl.fit,boy.fit)

# Graph CFA model

library(psych)

par(mfcol=c(1,2),pty="m",cex=.7)
lavaan.diagram(girl.fit,title="Girls Bi-Factor CFA")
lavaan.diagram(boy.fit, title = "Boys Bi-Factor CFA")





############################
# Optional Full Model
############################

# Run CFA combined sample
# Create a combined covariance matrix of girls and boys

comb = girls + boys


comb.fit = cfa(cfa2.model,sample.cov = comb,sample.nobs=445,std.lv=FALSE)
fitMeasures(comb.fit,fit.index)
summary(comb.fit, standardized = TRUE)


