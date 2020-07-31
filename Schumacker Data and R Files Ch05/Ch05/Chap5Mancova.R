# Chapter 5
# MANCOVA example (Stevens, 2009, p. 302)

# Install Packages

install.packages("MASS")      # MANOVA
install.packages("car")       # Type III SS
install.packages("psych")     # Descriptive statistics
install.packages("effects")	# Adjusted Means

# Load packages

library(MASS); library(car); library(psych)

# Input data 

group1 = matrix(c(1,15,17,3,1,10,6,3,1,13,13,1,1,14,14,8,1,12,12,3,1,10,9,9,1,12,12,3,1,8,9,12,1,12,15,3,1,8,10,8,1,12,13,1,1,7,11,10,1,12,16,1,1,9,12,2,1,12,14,8),nrow=15,ncol=4,byrow=TRUE)
group1

group2 = matrix(c(2,9,9,3,2,13,19,5,2,13,16,11,2,6,7,18,2,10,11,15,2,6,9,9,2,16,20,8,2,9,15,6,2,10,8,9,2,8,10,3,2,13,16,12,2,12,17,20,2,11,18,12,2,14,18,16),nrow=14,ncol=4,byrow=TRUE)
group2


mancova= data.frame(rbind(group1,group2))
names(mancova) = c("GPID","Precomp","Postcomp","Posthior")
attach(mancova)
mancova


# MANCOVA

options(scipen=999)

outcome = cbind(mancova$Postcomp,mancova$Posthior)
model = manova(outcome ~ GPID + Precomp + GPID * Precomp, data = mancova)

summary(model,test = "Wilks",type ="III")
summary(model,test = "Pillai",type = "III")
summary(model,test = "Hotelling",type = "III")
summary(model,test = "Roy",type = "III")

#####################################################
# Results reported by Stevens(2009), p. 303
# Ran separate model for the pretest effect
# Ran separate model for pretest + group effect
# See degrees of freedom differences in the output
#####################################################

model = manova(outcome ~ Precomp, data = mancova)
summary(model,test = "Wilks",type ="III")

model = manova(outcome ~ Precomp + GPID, data = mancova)
summary(model,test = "Wilks",type ="III")
#########################################################


#  Print original Dependent variable means

library(psych)
describeBy(mancova,mancova$GPID)

# Postcomp ANOVA and adjusted Dependent variable means

options(scipen=999)
factor(GPID)
modelA = aov(Postcomp ~ Precomp + GPID, data = mancova)
summary(modelA,type="III")

library(effects)
adjmeanA = effect("GPID",modelA,se=TRUE,xlevels=2)
summary(adjmeanA)
adjmeanA$se

# Posthior ANOVA and adjusted Dependent variable means

modelB = aov(Posthior ~ Precomp + GPID, data = mancova)
summary(modelB, type = "III")

adjmeanB = effect("GPID",modelB,se=TRUE,xlevels=2)
summary(adjmeanB)
adjmeanB$se


#  Postcomp Descriptive Statistics Males (n = 15) Females (n = 14)

mean(Postcomp[1:15]); sd(Postcomp[1:15]);mean(Precomp[1:15]) #GPID1
mean(Postcomp[16:29]); sd(Postcomp[16:29]);mean(Precomp[16:29]) #GPID2
mean(Postcomp); sd(Postcomp); mean (Precomp)	# Grand Mean


# Posthior  Descriptive Statistics 

mean(Posthior[1:15]); sd(Posthior[1:15]);mean(Precomp[1:15]) #GPID1
mean(Posthior[16:29]); sd(Posthior[16:29]);mean(Precomp[16:29]) #GPID2
mean(Posthior); sd(Posthior); mean (Precomp)


# Correlation between Dependent variables and pretest 

cor.test(Precomp,Postcomp)
cor.test(Precomp,Posthior) 

# Plot of correlation between Dependent variables and pretest

par(mfrow=c(2,2),usr=c(0,20,0,20))    # Dimension 2 by 2 graph and axis from 0 to 20 
plot(Precomp,Postcomp)
abline(modelA)
plot(Precomp,Posthior)
abline(modelB)








############################################################
Hand Calculation of Adjusted Means - Different Results
############################################################

# Regression approach using lm function

modelA = lm(Postcomp ~ GPID + Precomp -1, data = mancova)
summary(modelA,type="III")

modelB = lm(Posthior ~ GPID + Precomp -1, data = mancova)
summary(modelB, type = "III")

# Correlation between Precomp and Postcomp, Posthior

cor.test(mancova$Precomp,mancova$Postcomp)
cor.test(mancova$Precomp,mancova$Posthior)

#  Postcomp Adjusted Means

mean(Postcomp[1:15]); sd(Postcomp[1:15]);mean(Precomp[1:15]) #GPID1
mean(Postcomp[16:29]); sd(Postcomp[16:29]);mean(Precomp[16:29]) #GPID2
mean(Postcomp); sd(Postcomp); mean (Precomp)
cor(Precomp,Postcomp)

# Posthior Adjusted Means

mean(Posthior[1:15]); sd(Posthior[1:15]);mean(Precomp[1:15]) #GPID1 # GPID1
mean(Posthior[16:29]); sd(Posthior[16:29]);mean(Precomp[16:29]) #GPID2
mean(Posthior); sd(Posthior); mean (Precomp)
cor(Precomp,Postcomp)

