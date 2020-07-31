# Chapter 6 Doubly Multivariate Example
# Tabachnick and Fidell p. 368
# Mental Rotation Experiment
# group:  letter G = 1 or symbol = 2
# 1st DV (intercept):  avg reaction times over four angles of rotation
# 2nd DV (slope)    :  change in reaction time over four angles of rotation

# Read in data file from Internet

mydata = read.table(file = "http://media.pearsoncmg.com/ab/ab_tabachnick_multistats_6/datafiles/ASCII/dblmult.dat",header=TRUE,sep="\t")
attach(mydata)
mydata

# Compute means for groups

library(psych)
describeBy(mydata,group)

# Plot DV's across 4 angle of rotation sessions
# Intercept by group across 4 angle rotation sessions

install.packages("ggplot2")
library(ggplot2)

Grp =   matrix(rep(cbind("G","symbol"),c(4,4)),nrow=8,ncol=1)
session = matrix(rep(1:4,c(1)),nrow=8,ncol=1)
Mns = matrix(cbind(200.70,133.75,90.46,72.39,40.85,24.30,22.95,22.46),nrow=8,ncol=1)
intcp = data.frame(Grp,session,Mns)
intcp

line = ggplot(intcp,aes(x=session,y=Mns,group=Grp))
line + geom_line(aes(linetype=Grp)) + labs(list(title="Mean Intercept Across Angle Sessions",x="Angle Sessions",y="Mean Intercept"))

# Run multivariate repeated measures on Intercepts

options(scipen=999)
outintcp = manova(cbind(intrcpt1, intrcpt2, intrcpt3, intrcpt4)~ group, data = mydata)

summary(outintcp,test="Wilks")
summary(outintcp,test="Pillai")
summary(outintcp,test="Hotelling")
summary(outintcp,test="Roy")

#################################################
# Slope by group across 4 angle rotation sessions
#################################################

# Do not need to specify Grp and session again
# Grp =   matrix(rep(cbind("G","symbol"),c(4,4)),nrow=8,ncol=1)
# session = matrix(rep(1:4,c(1)),nrow=8,ncol=1)

Mnslope = matrix(cbind(642.62,581.13,516.87,505.39,654.50,647.53,568.22,535.82),nrow=8,ncol=1)
slp  = data.frame(Grp,session,Mnslope)
slp 

line = ggplot(intcp,aes(x=session,y=Mnslope,group=Grp))
line + geom_line(aes(linetype=Grp)) + labs(list(title="Mean Slope Across Angle Sessions",x="Angle Sessions",y="Mean Slope"))

# Run multivariate repeated measures for slopes

outslp = manova(cbind(slope1, slope2, slope3, slope4)~ group, data = mydata)
summary(outslp,test="Wilks")
summary(outslp,test="Pillai")
summary(outslp,test="Hotelling")
summary(outslp,test="Roy")

# lmer4 package alternative solution

install.packages("reshape")
library(reshape)

mydata2 = mydata[,c(1,3:10)]  # drop subject number for data set
ppmydata = melt(mydata2,id = "group",measured=c("slope1","intrcpt1","slope2","intrcpt2","slope3","intrcpt3","slope4","intrcpt4"))
head(ppmydata,n=20)  # print first 20 records

time = cbind(rep(c(1:4),c(40,40,40,40)))
id = cbind(rep(c(1:20),c(8)))

ppmydata = data.frame(cbind(id,ppmydata,time))
attach(ppmydata)
head(ppmydata,n=20)   #  check data set

# Conduct multivariate repeated measures 

install.packages("lmer4")
library(lme4)

lmer.ppout = lmer(value ~ group + time + group*time + (1|id), data=ppmydata)
anova(lmer.ppout)

pgroup = 1 - pf(.4035,1,159)  
pgroup

ptime = 1 - pf(3.0606,1,159)  
ptime

pgroup.time = 1 - pf(.2669,1,159)  
pgroup.time

###############################
# intercept only 
##############################

myintrcpt = ppmydata[c(21:40,61:80,101:120,141:160),]

lmer.intr = lmer(value~group + time + group*time+ (1|id), data = myintrcpt)
anova(lmer.intr)

options(scipen=999)

pgroup = 1 - pf(44.573,1,159)  
pgroup

ptime = 1 - pf(133.699,1,159)  
ptime

pgroup.time = 1 - pf(78.605,1,159)  
pgroup.time

###################################
# Slope only
####################################

myslope = ppmydata[c(1:20,41:60,81:100,121:140),]

lmer.pps = lmer(value~group + time + group*time+ (1|id), data = myslope)
anova(lmer.pps)

pgroup = 1 - pf(.5906,1,159)  
pgroup

ptime = 1 - pf(13.4538,1,159)  
ptime

pgroup.time = 1 - pf(.0267,1,159)  
pgroup.time


