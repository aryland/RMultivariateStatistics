# Chapter 12 Growth Modeling
# Raykov & Marcoulides (2008)
# Data set ch13ex1-mcm.dat
# 400 High School students; 9th to 12th grades
# College aspiration:  intercept and slope model
# School motivation:  Covariate(contintous)
# Predictors: Parental dominance, parental encouragement for progress

library(lavaan)

mns = c(20.602,21.688,22.730,23.760,98.795,99.240,50.002,49.940,50.040,49.943)
mns

advgrowth = data.frame(lower2full(c(
2.481,
2.510,4.466,
3.113,4.966,7.371,
3.488,5.722,7.933,10.318,
12.482,19.497,25.238,30.622,405.091,
 6.379,9.411,11.363,14.389,179.383,245.697,
 0.151,0.011,0.118,0.036,-1.045,0.308,0.945,
-0.014,0.335,0.049,0.018,0.594,0.115,0.083,1.114,
-0.012,0.100,0.412,0.075,-0.318,0.226,-0.013,0.078,0.926,
-0.015,-0.163,-0.221,-0.051,-0.420,0.493,0.028,0.032,0.057,1.002),diagonal=TRUE),row.names =c("COLLASP1","COLLASP2","COLLASP3","COLLASP4","PARSTYL1","PARSTYL2","MOTIVN1","MOTIVN2","MOTIVN3","MOTIVN4") )
names (advgrowth) =c("COLLASP1","COLLASP2","COLLASP3","COLLASP4","PARSTYL1","PARSTYL2","MOTIVN1","MOTIVN2","MOTIVN3","MOTIVN4")
advgrowth = as.matrix(advgrowth)
advgrowth

## linear growth model with a time-varying covariate and two predictors

advgrowth.model = '

  # intercept and slope with fixed coefficients

    i =~ 1*COLLASP1 + 1*COLLASP2 + 1*COLLASP3 + 1*COLLASP4
    s =~ 0*COLLASP1 + 1*COLLASP2 + 2*COLLASP3 + 3*COLLASP4

  # regressions     # Predictor variables - continuous  
     
    i + s ~  PARSTYL1    # Dropped non-significant PARSTYL2 predictor
    
  # time-varying covariates  
  
    COLLASP1 ~ MOTIVN1
    COLLASP2 ~ MOTIVN2
    COLLASP3 ~ MOTIVN3
    COLLASP4 ~ MOTIVN4'   

advgrowth.fit = growth(advgrowth.model, sample.cov=advgrowth, sample.nobs=400, sample.mean = mns)
summary(advgrowth.fit, standardized=TRUE)


# Other options for output

summary(advgrowth.fit, standardized=TRUE,fit.measures=TRUE,rsquare=TRUE)
inspect(advgrowth.fit,"user")   # Provides matrices and values estimated in model
fitted.values(advgrowth.fit)    # Estimated covariance matrix and means

# Savalei, V. & Rhemtulla, M. (2012). On obtaining estimates of the fraction of missing information from FIML. Structural Equation Modeling: A Multidisciplinary Journal, 19(3), 477-494. 

parameterEstimates(advgrowth.fit) # Lists all parameter estimates

 # constrain error variances to be equal  ML = 37.094, p = .04

    COLLASP1 ~~ r*COLLASP1
    COLLASP2 ~~ r*COLLASP2
    COLLASP3 ~~ r*COLLASP3
    COLLASP4 ~~ r*COLLASP4

