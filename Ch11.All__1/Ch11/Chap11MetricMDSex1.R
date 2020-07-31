# Chapter 11 MDS examples
# Note stats package is the base package
#
# Original data distance in miles between cities
#

#        BOS      DC   NY    LA    SC       SF
#BOS     0       439  215   2979   2976    3095
#DC      439      0   233   2631   2684    2799
#NY      215     233   0    2786   2815    2934
#LA     2979    2631 2786     0     384     382
#SC     2976    2684 2815    384     0       87
#SF     3095    2799 2934    382    87      0

library(stats)
library(psy)
library(MASS)

d = matrix(c(0,439,215,2979,2976,3095,439,0,233,2631,2684,2799,215,233,0,2786,2815,2934,2979,2631,2786,0,384,382,2976,2684,2815,384,0,87,3095,2799,2934,382,87,0),nrow=6,ncol=6,byrow=TRUE)
dimnames(d) = list(c("Boston","DC","NY","LA","SC","SF"), c("Boston","DC","NY","LA","SC","SF"))
d

# Euclidian distances computed

euclid = dist(d)
euclid

# Classical metric MDS

options(scipen=999)
fit = cmdscale(d,eig=TRUE, k=2) 	# k is the number of dimensions
fit 	

# Compute P2 value for model fit

P2 = sum(abs(fit$eig[1:2]))/sum(abs(fit$eig))
P2

# Compute Mardia criteria

Mardia = sum((fit$eig[1:2])^2)/sum((fit$eig)^2)
Mardia

# Eigenvalues

eigen(d)

#  Plot of data

x = cbind (- fit$points[,1])
y = cbind (- fit$points[,2])

plot(x, y, xlab="Dimension 1",ylab="Dimension 2", main = "Classical MDS",type="n",xlim=range(x)*1.2)
text(x,y,labels=colnames(d))

# United States Map

install.packages("maps")
library(maps)
map("state")

# Scree plot  

install.packages("psy")
library(psy)
scree.plot(d,title="Scree Plot",type = "R")

# Plot Shepard diagram 

dist_sh = Shepard(euclid,fit$points)
plot(dist_sh, pch = "*", xlab = "Dissimilarity", ylab = "Distance", xlim = c(0,7000), ylim = c(0,7000))
lines(dist_sh$x,dist_sh$y,type="s")


