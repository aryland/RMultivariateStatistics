# PCA basics
# Raykov & Marcoulides (2008) p. 217

S = matrix(c(44.23,22.42,31.23,32.54,19.22,22.42,55.13,22.55,25.56,23.15,31.23,22.55,61.21,29.42,27.35,32.54,25.56,29.42,57.42,33.34,19.22,23.15,27.35,33.34,72.36),nrow=5,ncol=5,byrow=FALSE)
S

# Convert S variance-covariance to R correlation matrix

R = cov2cor(S)
R

# Determinant of matrices

det(S)
det(R)

eigen(S)
eigen(R)

# Compute Identity matrix 
# Multiply eigenvector matrix by transpose of eigenvector matrix

out = (eigen(S)) 	# Place eigenvectors in a file
V = (out$vectors)     # Put only eigenvector values in a file
tV = t(out$vectors)   #  Transpose of eigenvectors of S
I = V %*% tV          # Multiply matrix of eigenvector values (vectors) by transpose
round(I,2)


###################################
# Compute Eigenvalues from matrices
###################################
options(scipen=999)
R = cov2cor(S)
R	
out2 = (eigen(R)) 	   # Place eigenvectors in a file
V = (out2$vectors)         # Put only eigenvector values in a file
tV = t(out2$vectors)       # Transpose of eigenvectors of R
E = tV%*%R%*%V             # Multiply matrices to compute eigenvalues
E = rbind(E[1,1],E[2,2],E[3,3],E[4,4],E[5,5])  # select eigenvalues on diagonal of matrix
E                          # Print out eigen values


# Identity matrix for correlations

out2 = (eigen(R)) 	     # Place eigenvectors in a file
E = (out2$vectors)         # Put only eigenvector values in a file
tE = t(out2$vectors)       #  Transpose of eigenvectors of R

I2 = E %*% tE               # Multiply matrix of eigenvector values (vectors) by transpose
round(I,2)


