# Chapter 12 Growth Model
# Schumacker & Lomax (2010, p. 343)
# 5 time periods - 168 adolescents, Age 11 to 15

library(lavaan)

basicgrowth = data.frame(lower2full(c(1,.161,1,.408,.348,1,.373,.269,.411,1,.254,.143,.276,.705,1),diagonal=TRUE), row.names =c("Age11","Age12","Age13","Age14","Age15"))
names(basicgrowth) = c("Age11","Age12","Age13","Age14","Age15")
basicgrowth = as.matrix(basicgrowth)
basicgrowth

# sample means

basicgrowth.mn = c(.201, .226, .326,.417,.446)
basicgrowth.mn

#  Linear Growth Model

# Model 1 - Intercept and Slope 

basic.model1 = '
# intercept
i =~ 1*Age11 + 1*Age12 + 1*Age13 + 1*Age14 + 1*Age15
# slope
s =~ 0*Age11 + 1*Age12 + 2*Age13 + 3*Age14 + 4*Age15'

basic.fit1 = growth(basic.model1,sample.cov=basicgrowth, sample.nobs=168,sample.mean = basicgrowth.mn)
summary(basic.fit1,standardized=TRUE)



# Model 2 - Intercept and Slope with residual variance constrained equal

basic.model2 = '

# intercept
i =~ 1*Age11 + 1*Age12 + 1*Age13 + 1*Age14 + 1*Age15

# slope
s =~ 0*Age11 + 1*Age12 + 2*Age13 + 3*Age14 + 4*Age15

# residual variance

Age11 ~~ r*Age11
Age12 ~~ r*Age12
Age13 ~~ r*Age13
Age14 ~~ r*Age14
Age15 ~~ r*Age15'


basic.fit2 = growth(basic.model2,sample.cov=basicgrowth, sample.nobs=168,sample.mean = basicgrowth.mn)
summary(basic.fit2,standardized=TRUE)


# Model 3 - Intercept and Slope with error variance correlated

basic.model3 = '

# intercept
i =~ 1*Age11 + 1*Age12 + 1*Age13 + 1*Age14 + 1*Age15

# slope
s =~ 0*Age11 + 1*Age12 + 2*Age13 + 3*Age14 + 4*Age15

# correlated error variance

Age11 ~~ Age12
Age14 ~~ Age15 '


basic.fit3 = growth(basic.model3,sample.cov=basicgrowth, sample.nobs=168,sample.mean = basicgrowth.mn)
summary(basic.fit3,standardized=TRUE)


# Optional output functions


inspect(fit,"user")   # Provides matrices and values estimated in model
fitted.values(fit)    # Estimated covariance matrix and means
coef(fit)		    #  coefficients slope and intercept


# Savalei, V. & Rhemtulla, M. (2012). On obtaining estimates of the fraction of missing information from FIML. Structural Equation Modeling: A Multidisciplinary Journal, 19(3), 477-494. 

parameterEstimates(fit)  #  Lists all parameter estimates




