#############################
# Chapter 10 exercise
# Install and load packages
############################

install.packages(psych)
install.packages("rela")
install.packages(MASS)
install.packages("parallel")
library(psych);library(rela);library(MASS);library(parallel)

options(scipen=999)   # Report decimal values

###################################################################
# Read Data File
# Use file.choose() to find attitude file
# Read in mydata file
# mydata = read.table(file= "C:/attitude.txt",header=TRUE,sep=" ")
###################################################################

# Optional use of file.choose() to open dialog window and select file
# 7 variable names for 30 subjects
# rating, complaints, privileges, learning, raises, critical, advance

mydata = read.table(file.choose(), header = TRUE, sep =" ")
pcdata = mydata[,-1]  # delete id
pcdata=as.matrix(pcdata)

# Covariance matrix

mycov = cov(pcdata)
mycov

#################################
# Basic Check on Suitable PCA
# Use raw data file pcdata
#################################

paf.pc = paf(pcdata,eigcrit=1,convcrit=.001)
summary(paf.pc)

cortest.bartlett(pcdata,n = 30)

det(mycov)

##################################
# Principal Component Analysis
##################################

pcmodel = principal(mycov,nfactors=5,rotate="none")  # no scores
pcmodel

# Cronbach Alpha

alpha(mycov)


##################################
# Scree Plot
##################################

plot(pcmodel$values, type = "b", xlim=c(1,10),main = "Scree Plot",xlab="Number of Factors",ylab="Eigenvalues")

#  Factor Analysis Model

fa.diagram(pcmodel)

# Principal component eigenvector estimates

pcaout = eigen(mycov) 	# Place eigenvectors in a file
V = (pcaout$vectors)    # Put only eigenvector values in a file
tV = t(pcaout$vectors)     # Transpose of eigenvectors of S
tV
Identity = V %*% tV               # Multiply matrix of eigenvector values (vectors) by transpose
round(Identity,2)



# Compute PC scores

Y1 = -0.44672*rating + -0.520624*complaints + -0.375773*privileges + -0.42100*learning + -0.37625*raises + -0.130030*critical + -0.229074*advance
Y2 =  0.42184*rating +  0.372077*complaints + -0.076327*privileges + -0.14567*learning + -0.23340*raises + -0.398290*critical + -0.665922*advance
Y3 = -0.24003*rating + -0.143226*complaints +  0.651322*privileges +  0.18648*learning + -0.22392*raises + -0.633035*critical +  0.109576*advance
Y4 =  0.12619*rating + -0.108109*complaints + -0.626328*privileges +  0.48514*learning +  0.10410*raises + -0.517067*critical +  0.257973*advance
Y5 =  0.20102*rating + -0.372379*complaints +  0.077836*privileges +  0.62078*learning + -0.44682*raises +  0.377987*critical + -0.294907*advance
Y6 =  0.47249*rating +  0.022005*complaints + -0.018141*privileges + -0.30156*learning + -0.59329*raises +  0.018067*critical +  0.576784*advance
Y7 =  0.53413*rating + -0.647424*complaints +  0.173423*privileges + -0.23474*learning +  0.43742*raises + -0.114743*critical + -0.076591*advance


#  Identity Matrix from PC scores

file = matrix(c(Y1,Y2,Y3,Y4,Y5,Y6,Y7),nrow=30,ncol=7)
round(cor(file),2)





