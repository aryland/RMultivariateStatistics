# Chapter 8 Canonical r Exercise Tabachnick & Fidell p. 572

Xvar = matrix(cbind(1,7,4.6,1,7,7,7,7,1,1,5.6,6.6,4.9,7,1,1),8,2,byrow=FALSE)
Xvar = data.frame(Xvar)
names(Xvar) = c("TS","TC")
Xvar


Yvar = matrix(cbind(1,7,7,1,7,6.4,7,2.4,1,1,7,5.9,2.9,3.8,1,1),8,2,byrow=FALSE)
Yvar = data.frame(Yvar)
names(Yvar) = c("BS","BC")
Yvar

# Install the CCA package

install.packages("CCA")
library(CCA)

# Compute four matrices

matcor(Xvar,Yvar)

# Run the canonical correlation

belly = cc(Xvar,Yvar)
belly

# Plot the dimensions

plt.cc(belly,type="i")

# F test of canonical variates

install.packages("yacca")
library(yacca)

options(scipen=999)	
belly.fit = cca(Xvar,Yvar)
F.test.cca(belly.fit)

# Compute standardized canonical coefficients  
# Top measured X variables

bellyXsd = diag(sqrt(diag(cov(Xvar))))
outX = matrix(bellyXsd%*%belly$xcoef,nrow=2,ncol=2,byrow=FALSE,dimnames=list(c("TS","TC"),c("CV1","CV2")))

# Bottom measured Y variables 

bellyYsd = diag(sqrt(diag(cov(Yvar))))
outY = matrix(bellyYsd%*%belly$ycoef,nrow=2,ncol=2,byrow=FALSE,dimnames=list(c("BS","BC"),c("CV1","CV2")))






