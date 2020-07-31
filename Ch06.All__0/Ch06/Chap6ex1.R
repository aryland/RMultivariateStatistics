# Chapter 6 Example 1 - Single DV 3 repeated measurements
# Linear Mixed Model for Repeated Measures

install.packages("nlme")
install.packages("reshape")
install.packages("ez")
install.packages("psych")

library(nlme)
library(reshape)
library(ez)
library(psych)

# Eduction data in vectors then data frame

test1 = rbind(6,9,4,3,1,7,8,9,8,6)
test2 = rbind(12,14,8,10,6,15,8,11,12,10)
test3 = rbind(18,16,15,12,10,20,15,18,13,16)
names = rbind("Bill","Mary","Sue","Jill","Cary","Juan","Manuel","Beth","Martin","Robert")
student = data.frame(names,test1,test2,test3)


# Create Data Frame as a person period (subject x repeat) data format
# Function nlme and aov requires this person-period data format


myfile = melt(student, id = "names", measured = c("test1","test2","test3"))
myfile

# Conduct multivariate repeated measures - single dependent variable using lme() and ezANOVA

attach(myfile)    			# use names in file
variable = factor(variable)		# delcare variable a factor - group level variable

out = lme(value ~ variable,data = myfile, method="ML",random = ~1 | names)
summary (out)

aov_out = ezANOVA(data=myfile, dv= .(value),wid= .(names),within = .(variable), detailed=TRUE,type=3)
aov_out

# provide descriptive statistics by test

testfile = (myfile[2:3])            # Drop names; create new file
describeBy(testfile,group=variable)

## OR

mean(test1);sd(test1)
mean(test2);sd(test2)
mean(test3);sd(test3)















