# MDS example Everitt voting behavior

install.packages("HSAUR")
library(HSAUR)

data("watervoles", package = "HSAUR")
voles_mds <- cmdscale(watervoles, k = 13, eig = TRUE)
voles_mds$eig

# P2 is the sum of eigenvalue 1 and eigenvalue 2 divided by the total sum of all eigenvalues

P2 = sum(abs(voles_mds$eig[1:2]))/sum(abs(voles_mds$eig))
P2

# Mardia is the sum of the squared eigenvalues divided by the square of the total eigenvalues

Mardia = sum((voles_mds$eig[1:2])^2)/sum((voles_mds$eig)^2)
Mardia

# Plot of data

x <- voles_mds$points[,1]
y <- voles_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",xlim = range(x)*1.2, type = "n")
text(x, y, labels = colnames(watervoles))

################
# non-Metric MDS
################

install.packages("MASS")
library(MASS)

data("voting", package = "HSAUR")
voting_mds <- isoMDS(voting)


# Plot minimum spanning tree 

install.packages("ape")
library(ape)

st <- mst(watervoles)
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",xlim = range(x)*1.2, type = "n")

for (i in 1:nrow(watervoles)) {

w1 <- which(st[i, ] == 1)
segments(x[i], y[i], x[w1], y[w1])
 }

text(x, y, labels = colnames(watervoles))


# Plot ordinal midpoints of proximities

x <- voting_mds$points[,1]
y <- voting_mds$points[,2]

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",xlim = range(voting_mds$points[,1])*1.2, type = "n")

text(x, y, labels = colnames(voting))
voting_sh <- Shepard(voting[lower.tri(voting)],voting_mds$points)

# Plot Shepard diagram of ordinal proximities

plot(voting_sh, pch = "*", xlab = "Dissimilarity", ylab = "Distance", xlim = range(voting_sh$x), ylim = range(voting_sh$x))
lines(voting_sh$x, voting_sh$yf, type = "S")


