###################################
non-metric MDS
###################################
#  Uses psych package
#  data is 16 multiple choice items on 1525 subjects from SAPA assessment
# Install packages
 
install.packages("MASS")
install.packages("psych")
library(MASS)
library(psych)

# Read in Data 

data(iqitems)
describe(iqitems)

#  omit missing values

fiqitems = na.omit(iqitems)
names = c("r_4","r_16","r_17","r_19","l_7","l_33","l_34","l_58","m_45","m_46","m_47","m_55","ro_3","ro_4","ro_6","ro_8")
describe(fiqitems)


# compute distance matrix 

MDScor = cor(fiqitems)
distNMDS = cor2dist(MDScor)
distNMDS2 = dist(distNMDS)  # lower triange distance matrix required for Shepard function

nonMDS2 =  isoMDS(distNMDS, k = 2)    # k is the number of dimensions
nonMDS2				       # view results


nonMDS4 =  isoMDS(distNMDS, k = 4)    # k is the number of dimensions
nonMDS4				        # view results

# Plot of non-metric eigenvectors

non1 = cbind (nonMDS4$points[,1])
non2 = cbind (nonMDS4$points[,2])

plot(non1,non2, xlab="Dimension 1",ylab="Dimension 2", main = "non-Metric MDS",type="n",xlim=range(non1)*1.2,ylim=range(non2)*2)
text(non1,non2,labels=names)

# Scree plot  

install.packages("psy")
library(psy)
scree.plot(distNMDS,title="Scree Plot non-Metric MDS",type = "R")

# Plot Shepard diagram 

sh_nmds = Shepard(distNMDS2,nonMDS4$points)

plot(sh_nmds, pch = "*", xlab = "Dissimilarity", ylab = "Distance",main="Shepard Diagram")
lines(sh_nmds$x,sh_nmds$y,type="s")

