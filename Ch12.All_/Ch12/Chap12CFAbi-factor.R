# Chapter 12 CFA bi-factor model
# Covariance matrix Schumacker & Lomax (2010, p. 171)

library(lavaan)

fullcov = data.frame(lower2full(c(49.064,9.810,22.182,27.928,14.482,81.863,9.117,2.515,5.013,12.196,10.610,3.389,3.605,13.217,26.645,19.166,6.954,13.716,18.868,28.502,58.817),diagonal=TRUE),row.names= c("Visperc","Cubes","Lozenges","Parcomp","Sencomp","Wordmean"))
names(fullcov) = c("Visperc","Cubes","Lozenges","Parcomp","Sencomp","Wordmean")
fullcov

# Declare matrix with row and column names for computations

fullcov = as.matrix(fullcov)

# Specify CFA bi-factor model

cfa2.model = '
	spatial=~ Visperc + Cubes + Lozenges
	verbal =~ Parcomp + Sencomp + Wordmean
	spatial~~verbal'

cfa.fit = cfa(cfa2.model,sample.cov=fullcov,sample.nobs=301,std.lv=TRUE)

summary(cfa.fit)

# Model modification

modindices(cfa.fit)

# Mdified Final Model

mcfa2.model = '
	spatial =~Visperc + Cubes + Lozenges
	verbal =~ Parcomp + Sencomp + Wordmean
	spatial~~verbal
     Cubes~~Lozenges'     # Added error covariance
   
mcfa.fit = cfa(mcfa2.model,sample.cov=fullcov,sample.nobs=301,std.lv=TRUE)
summary(mcfa.fit)

# Graphing final model

library(psych)

structure.diagram (mcfa2.model,  


modindices(mcfa.fit)

# Final Model

fcfa2.model = '
	spatial =~Visperc + Cubes + Lozenges
	verbal =~ Parcomp + Sencomp + Wordmean
	spatial~~verbal
     Cubes~~Lozenges     # Added error covariance based on modification indices
     Parcomp~~Wordmean'   # Added error covariance 

fcfa.fit = cfa(fcfa2.model,sample.cov=fullcov,sample.nobs=301,std.lv=TRUE)
summary(fcfa.fit)

# Graphing Final Model

library(psych)

lavaan.diagram(fcfa.fit,title ="CFA Bi-Factor Confirmatory Model")

