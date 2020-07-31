##########################################
# First Canonical Correlation in R
# Use stat package
# Read data set
##########################################

install.packages("stats")
data(LifeCycleSavings)

# Create x and y variables

pop = LifeCycleSavings[,2:3]
oec = LifeCycleSavings[,-(2:3)]

# View x and y variables

pop
oec

# Look at basic cancor function in stat package

help(cancor)

# Compute canonical correlation between set of x and y variables

cancor(pop,oec)

##############################################################
#  Second Canonical Correlation
#  Use CCA package
#  Correlations within and between the two sets of variables
##############################################################

install.packages("CCA")
require(CCA)
matcor(pop,oec)

cc1 = cc(pop,oec)

# Display canonical coefficients

cc1$cor

# Display raw canonical coefficients (unstandardized)

cc1[3:4]

# Canonical loadings

cc2 = comput(pop,oec,cc1)
cc2[3:6]

# Test of canonical variates

v = (1-cc1$cor^2)
n = dim(pop)[1]
p = length(pop)
q = length(oec)
k = min(p,q)
m = n - 3/2 - (p + q) /2

# Initialize and compute values

w = rev(cumprod(rev(v)))
d1 = d2 = f = vector("numeric",k)
for (i in 1:k) {
    s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
}

# Assign to table and print

pv = pf(f, d1, d2, lower.tail = FALSE)
(dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))

# First Standardized canonical coefficients for interpretation

s1 = diag(sqrt(diag(cov(pop))))
s1 %*% cc1$xcoef


# Second Standardized canonical coefficients for interpretation

s2 = diag(sqrt(diag(cov(oec))))
s2 %*% cc1$ycoef







 