# Chapter 11 Exercise 6
################################################################
# psych package 
# burt data set - 11 Emotional variables in correlation matrix 
# Burt (1915)
#################################################################

library(MASS)
library(psych)
library(psy)

#  Correlation Matrix 

data(burt) 
names=c("social","sorrow","tender","joy","wonder","elation","disgust","anger","sex","fear","subjection")
burt

# Scree plot from correlation matrix

scree.plot(burt,title="Classical Scree Plot",type="R")

# Compute classical MDS
# Uses proximity matrix from dist() function

distburt = dist(burt)   # proxmity matrix for cmdscale() function

burtmds=cmdscale(distburt,eig=TRUE,k=2)
burtmds

# Plot 11 Emotional variables

x = cbind (- burtmds$points[,1])
y = cbind (- burtmds$points[,2])

plot(x, y, xlab="Dimension 1",ylab="Dimension 2", main = "Classical MDS",type="n",xlim=range(x)*1.2)
text(x,y,labels = colnames(burt))

# Plot Shepard diagram 

dist_burt = Shepard(distburt,burtmds$points)
plot(dist_burt, pch = "*", xlab = "Dissimilarity", ylab = "Distance", xlim = c(0,4), ylim = c(0,4), main="Shepard Diagram")
lines(dist_burt$x,dist_burt$y,type="s")






