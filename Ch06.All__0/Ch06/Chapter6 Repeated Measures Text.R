test1 = rbind(6,9,4,3,1,7,8,9,8,6)
test2 = rbind(12,14,8,10,6,15,8,11,12,10)
test3 = rbind(18,16,15,12,10,20,15,18,13,16)
names = rbind("Bill","Mary","Sue","Jill","Cary","Juan","Manuel","Beth","Martin","Robert")
student = data.frame(names,test1,test2,test3)

install.packages("reshape")
library(reshape) 

myfile = melt(student, id = "names", measured = c("test1", "test2", "test3"))
myfile

install.packages("nlme")
library(nlme)

attach(myfile)
variable = factor(variable)

out = lme(value ~ variable, data = myfile, method="ml", random = ~1 names)
summary (out)

install.packages("ez-package")
library(ez-package)

install.packages("ezANOVA")
library(ezANOVA)

aov_out = ezANOVA(data=myfile, dy= .(value),wid= .(names), within = .(variable), detailed=TRUE, type=3)
aov_out

install.packages("psych")
library(psych)
testfile = (myfile[2:3])
describeBy(testfile, group=variable)

mean(test1);sd(test1)
mean(test2);sd(test2)
mean(test3);sd(test3)

read = rbind(7,8,5,6,7,,4,6,5,6,4,3,5,4,7,6)
dance = rbind(10,9,10,10,8,4,4,5,6,5,1,3,2,1,3)
tv = rbind(6,5,5,6,7,4,5,5,6,6,1,1,2,2,3)
ski = rbind(5,7,8,8,9,4,3,6,7,5,2,5,5,4,3)
names = rep(rbind("belly","politic","Admin"),c(5,5,5))

multdiv= data.frame(names,read,dance,tv,ski)
multdiv
