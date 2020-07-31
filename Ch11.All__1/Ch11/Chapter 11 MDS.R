install.packages("shinyDND")

available.packages()

install.packages("SensoMineR")
library(SensoMineR)
install.packages("smacof")
library(smacof)
install.packages("ape")
library(ape)
install.packages("ade4")
library(ade4)
install.packages("ecodist")
library(ecodist)
install.packages("labdsu")
library(labdsu)

library(stats)


help.search("multidimensional scaling")

help(package="MASS")

d = matrix(c(0,439,215,2979,2976,3095,439,0,233,2631,2684,2799,215,23,0,2786,2815,2934,2979,2631,2786,0,384,382,2976,2684,2815,384,0,87,3095,2799,2934,382,87,0),nrow=6,ncol=6,byrow=TRUE)

dimnames(d) = list(c("Boston","DC","NY","LA","SC","SF"),c("Boston","DC","NY","LA","SC","SF"))
d

euclid = dist(d)
euclid

options (scipen=999)
fit = cmdscale(d,eig=TRUE, k=2)
fit

P2 = (sum(abs(fit$eig[1:2]))/sum(abs(fit$eig)))
P2

Mardia = sum((fit$eig[1:2])^2) / sum((fit$eig)^2)
Mardia

x = cbind (- fit$points[,1])
y = cbind (- fit$points[,2])

plot(x, y, xlab = "Dimension 1", ylab = "Dimension 2",main = "Classical MDS", xlim=range(x)*1.2)
text(x, y, labels = colnames(d))

install.packages("maps")
library(maps)
map("state")

eigen(d)

install.packages ("psy")
library(psy)
scree.plot(d,title="Scree Plot",type = "R")

install.packages("MASS")
library(MASS)
help(package="MASS")

install.packages(“name_of_package”, repo = "https://mac.R-project.org")


dist_sh = Shepard(euclid, fit$points)
dist_sh

plot(dist_sh, pch = "*", xlab = "Dissimilarity", ylab = "Distance", xlim = c(0,7000), ylim = c(0,7000))
lines(dist_sh$x,dist_sh$y,type="s")

install.packages("MASS")
install.packages("psych")
library(MASS)
library(psych)

iqitems = read.csv(file = "iqitems.csv")
data(iqitems)
describe(iqitems)

fiqitems = na.omit(iqitems)
names = c("r_4","r_16","r_17","r_19","l_7","l_33","l_34","l_58","l_58","m_45","m_46","m_47","m_55","ro_3","ro_4","ro_6","ro_8")
describe(fiqitems)

MDScor = cor(fiqitems)
distNMDS = cor2dist(MDScor)
distNMDS2 = dist(distNMDS)

nonMDS2 = isoMDS(distNMDS, k = 2)
nonMDS2



nonMDS4 = isoMDS(distNMDS, k = 4)
nonMDS4

non1 = cbind (nonMDS4$points[,1])
non2 = cbind (nonMDS4$POINTS[,2])

plot(non1,non2, xlab="Dimension 1",ylab="Dimension 2", main = "non-Metric MDS",TYPE="n",xlim=range(non1)*1.2)
text(non1,non2,labels=names)


install.packages("psy")
library(psy)

scree.plot(distNMDS,title="Scree Plot Nonmetrics MDS",type = "R")

sh_nmds = Shepard(distNMDS2, nonMDS4$points)

plot(sh_nmds, pch = "*", xlab = "Dissimilarity", ylab = "Distance",main="Shepard Diagram")
lines(sh_nmds$x,sh_nmds$y,type="s")
