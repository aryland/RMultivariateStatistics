group = g1(2,6,labels=c("at-risk","not at-risk"))
math = c(2,3,5,2,4,6,5,7,8,10,9,7)
english = c(3,4,4,5,8,7,6,6,7,8,5,6)

discrim = data.frame(group, math, english)
discrim


install.pacKages("MASS")
library(MASS)
out = lda(group~ math + english, data = discrim)
out

install.packages("biotools")
library(biotools)
options(scipen=999)

factor(discrim[,1])
boxM(discrim[2:3],discrim[,1])

result = predict(out)$class
result = cbind(result)
prior = cbind (group)
final = data.frame(prior,result)
final

ct = table(prior, result)
diag(prop.table(ct,))

sum(diag(prop.table(ct)))

mytable = table(group,result)
mytable

prop.table(mytable)

attach(final)
chisq.test(group,result,correct=FALSE)

install.packages("MASS")
library(MASS)

Group = g1(3,10,labels=c("CBT","BT","NT"))
Actions = c(5,5,4,4,5,3,7,6,6,4,4,4,1,1,4,6,5,5,2,5,4,5,5,4,6,4,7,4,6,5)
Thoughts = c(14,11,16,13,12,14,12,15,16,11,14,15,13,14,15,19,13,18,14,17,13,15,14,14,13,20,13,16,14,18)
OCD = data.frame(Group, Actions, Thoughts)
attach(OCD)
OCD

install.packages("biotools")
library(biotools)
options(scipen=999)

factor(OCD[,1])
boxM(OCD[2:3],OCD[,1])

fit = lda(Group ~ Actions + Thoughts, data = OCD, prior = c(1,1,1)/3,na.action="na.omit")
fit

result = predict(fit)$class
result = cbind(result)
prior = cbind (Group)
out = data.frame(prior,result)
out

ct = table(prior, result)
diag(prop.table(ct))
sum(diag(prop.table(ct)))

mytable = table(Group,result)
mytable
prop.table(mytable)

chisq.test(Group, result)

GRP=cbind(c(rep(1,10),rep(2,10),rep(3,10)))

install.packages("CCA")
library(CCA)
outfit = cca(OCD[2:3],GRP)
summary(outfit)