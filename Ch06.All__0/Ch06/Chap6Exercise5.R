#  Chapter 6 Exercise 5
#  Raykov and Marcoulides p. 168-179
#  http://www.psypress.com/books/details/9780805863758/
#  Download all data files
#  Data set:  ch5ex3.dat
#  Gender male(0), n = 50 ; female (1), n = 111
#  DV: Induction reasoning - repeated 4 times

# specify directory where file has been saved

setwd("C:/")

# read in data set and attach data set

ch5ex3 = read.table(file="ch5ex3.dat",header=TRUE,sep="\t")
attach(ch5ex3)
head(ch5ex3)


# Descriptive statistics

install.packages("psych")
library(psych)

describeBy(ch5ex3,gender)

# Create person-period data set

install.packages("reshape")
library(reshape)

ppch5ex3 = melt(ch5ex3,id="gender",measured=c("ir.1","ir.2","ir.3","ir.4"))
time = cbind(rep(c(1:4),c(161,161,161,161)))
id = cbind(rep(c(1:161),c(4)))

ppch5ex3 = data.frame(cbind(id,ppch5ex3,time))
attach(ppch5ex3)
head(ppch5ex3,n=10)  # check data was created correctly


# Conduct multivariate repeated measures 

install.packages("lmer4")
library(lme4)

lmer.out = lmer(value ~ gender + time + gender*time + (1|id), data=ppch5ex3)
anova(lmer.out)

# significance of F values

pgender = 1 - pf(2.2310,1,159)  
pgender

ptime = 1 - pf(306.9423,1,159)
ptime
 
pgender.time = 1 - pf(1.6349,1,159)
pgender.time

# means and standard deviations for time variable

mean(ir.1);sd(ir.1)
mean(ir.2);sd(ir.2)
mean(ir.3);sd(ir.3)
mean(ir.4);sd(ir.4)

########################################

#  F functions:  F value at .05 alpha

F.05 = qf(.95,df1 = 1, df2 = 159)   # .05 alpha
p.F.05 = pf(3.90061,1,159)


