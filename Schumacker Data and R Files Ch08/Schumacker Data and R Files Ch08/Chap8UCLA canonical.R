#  UCLA canonical correlation example
#  http://www.stats.ucla.edu/stat/r/dae/canonical.htm
#  psychological variables = academic variables

# Read in data set from UCLA website


mm = read.csv("http://www.ats.ucla.edu/stat/data/mmreg.csv")
colnames(mm) = c("Control", "Concept", "Motivation", "Read", "Write", "Math", "Science","Sex")

# A list of the first ten record lines is given by:

head(mm,10)

#  Create two separate X and Y matrices (no gender variable)

psych = mm[, 1:3]
acad = mm[, 4:7]

# Correlation matrices Rxx, Ryy, and Rxy

matcor(psych,acad)

# Run canoncial correlation analysis

install.packages("CCA")
library(CCA)

cc1 = cc(psych,acad)
cc1

# Plot first dimension

plt.cc(cc1,type="v",var.label=TRUE)


# Compute canonical variate loadings 

cc2 = comput(psych,acad,cc1)
cc2

# Compute F test of canonical variates

install.packages("yacca")
library(yacca)
options(scipen=999)
cca2.fit = cca(psych,acad)
F.test.cca(cca2.fit)

# Compute standardized canonical loadings

# Psychological variables

psychsd =  diag(sqrt(diag(cov(psych))))
Xout = matrix(psychsd%*%cc1$xcoef,nrow=3,ncol=3,byrow=FALSE,dimnames = list(c("control","concept","motivation"),c("CV1","CV2","CV3")))

# Academic variables

acadsd = diag(sqrt(diag(cov(acad))))
Yout = matrix(acadsd%*%cc1$ycoef,nrow=4,ncol=3,byrow=FALSE,dimnames = list(c("read","write","math","science"),c("CV1","CV2","CV3")))

