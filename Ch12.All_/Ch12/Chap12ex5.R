# Chapter 12 Growth Model Exercise 5
# 4 time periods - 100 students Math Achievement

library(lavaan)

LGM = data.frame(lower2full(c(1,.64,1,.59,.67,1,.45,.57,.62,1), diagonal=TRUE),row.names =c("Math1","Math2","Math3","Math4"))
names(LGM) = c("Math1","Math2","Math3","Math4")
LGM = as.matrix(LGM)
LGM

# sample means

LGMmn = c(2.3,2.6,2.7,2.9)
LGMmn

#  Linear Growth Model

# Model 1 - Intercept and Slope 

LGM.model = '
i =~ 1*Math1 + 1*Math2 + 1*Math3 + 1*Math4
s =~ 0*Math1 + 1*Math2 + 2*Math3 + 3*Math4'

LGM.fit = growth(LGM.model,sample.cov=LGM, sample.nobs=100,sample.mean = LGMmn)
summary(LGM.fit,standardized=TRUE)


