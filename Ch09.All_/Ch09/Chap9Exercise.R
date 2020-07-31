# Chapter 9 Exercise
# Install package

install.packages("psych")
library(psych)

# Locate data sets in psych package

data()  # global - lists all data sets
data(package = "psych")

# Input correlation matrix

data(Harman.8)  #  Correlatins of 8 physical variables, 305 girls (Harman, 1976)
Harman.8        # Print correlation matrix

# Scree Plot

fa.parallel(Harman.8, n.obs=305, fm="minres",fa="fa")

# one and two factor solutions

options(scipen=999)

efa2 = fa(Harman.8,nfactors=2,n.obs = 305,rotate="none",fm="minres")
print(efa2)

efa3 = fa(Harman.8,nfactors=3,n.obs = 305,rotate="none",fm="minres")
print(efa3)





