# Chapter 4
# Factorial Manova
# Stevens p. 281 from Barcikowski (1983)

app = rep(c("1","2","3"),c(15,15,15))
method = rep(c("1","2","3","4","5"),c(3,3,3,3,3))
attit = c(15,9,NA,19,12,12,14,9,14,19,7,6,14,14,18,18,8,6,25,24,26,29,28,NA,11,14,8,18,11,NA,11,16,NA,13,10,NA,17,7,7,15,13,7,17,13,9)
achiev = c(11,7,NA,11,9,6,13,9,15,14,8,6,16,8,16,13,11,6,24,23,19,23,26,NA,14,10,7,17,13,NA,9,15,NA,11,11,NA,10,9,9,9,13,7,12,15,12)

app = factor(app)
method = factor(method)
mydata = data.frame(app,method,attit,achiev)
mydata
newdata = na.omit(mydata)  

# Factorial MANOVA

fit = manova(cbind(attit,achiev)~app*method,data=newdata)

summary(fit,test="Pillai")
summary(fit,test="Wilks")
summary(fit,test="Hotelling")

# Univariate Factorial F's

fit1 = aov(attit~app*method,data=newdata)
summary(fit1)

fit2 = aov(achiev~app*method,data=newdata)
summary(fit2)


# Means
install.packages("psych")
library(psych)
describeBy(newdata,newdata$app)
describeBy(newdata,newdata$method)

# Grand Means
summary(attit)
summary(achiev)

