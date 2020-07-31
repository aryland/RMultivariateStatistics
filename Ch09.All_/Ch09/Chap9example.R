# Chapter 9 Exploratory Factor analsysis
# Attitude toward Research 30 items and 541 subjects

# Install and load packages for factor analysis

install.packages("corpcor")  # partial correlation matrix
install.packages("GPArotation") # rotation of factors
install.packages("psych")   # factor analysis
install.packages("rela")   # iteman1() - Cronbach alpha and item descriptives paf() Bartlett and KMO
install.packages("MASS")   # Factor analysis
install.packages("parallel")  # Plot routines
library(corpcor);library(GPArotation);library(psych);library(rela)
library(MASS);library(parallel)

# file.choose()  - alternative find and select command

setwd("c:/")	# set file location - root directory

# Read in data file

factdat=read.table(file="atr30.csv",sep=",",header=TRUE)
factdat= as.matrix(factdat)   # specify as matrix for use in other packages
head(factdat,10)
tail(factdat,10)


# Use correlation matrix rather than raw data file

corfact = cor(factdat)
round(corfact,3)    # round correlations to three decimal places

# Prints out the p-values for correlations in matrix

print(corr.p(cor(factdat),541))


# Run Bartlett and KMO test in rela package with paf() 

paf.corfact = paf(factdat,eigcrit=1,convcrit=.001)
summary(paf.corfact)

# Test significance of the Bartlett test

cortest.bartlett(corfact, n = 541)

#  Alternative tests

cortest.mat(corfact, n1 = 541)	            # Alternative test
cortest.normal(corfact,n1=541,fisher=TRUE)   	# Steiger test


# Determinant of the correlation matrix

options(scipen=999)
det(corfact)

# Cronbach reliability
itemanal(corfact)
itemanal(corfact[,-30])   # Cronbach Alpha, item mean, sd



# Factor analysis using psycy package
# Two factors

options(scipen=999)
EFAwls2 = fa(corfact,nfactors=2,n.obs =541,rotate = "none",fm="wls")
print(EFAwls2)

# Three factors

EFAwls3 = fa(corfact,nfactors=3,n.obs =541,rotate = "none",fm="wls")
print(EFAwls3)

Scree Plot

plot(EFAwls2$values, type = "b", xlim=c(1,10),main = "Scree Plot",xlab="Number of Factors",ylab="Eigenvalues")
fa.parallel(corfact, n.obs=541, fm="wls",fa="fa")


# Orthogonal factor structure

EFAwlsortho = fa(corfact,nfactors=3,n.obs =541,rotate = "varimax",fm="wls")
print(EFAwlsortho)

# Oblique factor structure

EFAwlsobliq = fa(corfact,nfactors=3,n.obs =541,rotate = "oblimin",fm="wls")
print(EFAwlsobliq)

#  Other factor solutions  nfactor=2


EFAres = fa(corfact,nfactors=2,n.obs =541,rotate = "none",fm="minres")
print(EFAres)

# Other factor solutons nfactor = 3

EFAresortho = fa(corfact,nfactors=3,n.obs =541,rotate="varimax",fm="minres")
print(EFAresortho)

EFAresobliq = fa(corfact,nfactors=3,n.obs =541,rotate = "oblimin",fm="minres")
print(EFAresobliq)


##########################
# Factor Scores using psych package
# Two factor solution
# Use raw data fle for factor scores
#############################
# scores ="Anderson"
# scores = "Bartlett"

EFAwls2 = fa(factdat,nfactors=2,n.obs =541,rotate = "none",fm="wls",scores="Thurstone")
head(print(EFAwls2$scores),10)

# compute scale score

scores = sort(EFAwls2$scores,decreasing=TRUE)
round(scores,3)    # round scores to three decimal places
head(scores,5)
tail(scores,5)

#  Use high and low scores in formula
#   s = (100)/(3.3082 -(-3.0387)) = 100 / 6.3469 = 15.756
#   m = (0) - (-3.0387 * 15.756) = 47.877

# compute scaled scores

scaled = 47.877 + (15.756 * EFAwls2$scores)
round(scaled,3)

# Graph facor scores and scaled scores

hist(EFAwls2$scores)

hist(scaled)

#  Plot factor scores

par(mfrow=c(1,2))
fa.diagram(EFAwls2)
plot(EFAwls2)







