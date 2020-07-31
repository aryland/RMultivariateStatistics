# Chapter 8 example
# data set for cancor() example

data(LifeCycleSavings)
head(LifeCycleSavings,10)

# Run canonical correlation

pop <- LifeCycleSavings[, 2:3]    # select pop15 and pop75
oec <- LifeCycleSavings[, -(2:3)] # select all others
cancor(pop, oec)

# CCA package for F-test, varimax rotation, graphs

install.packages("CCA")
library(CCA)
matcor(pop,oec)

res.cc=cc(pop,oec)
plt.cc(res.cc,type="i")

# yacca package

install.packages("yacca")
library(yacca)
options(scipen=999)
cca.fit = cca(pop,oec)
F.test.cca(cca.fit)


