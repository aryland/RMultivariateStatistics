#  Chapter 6  Several Dependent variables - profile analysis
# Tabachnick & Fidell (2007) p. 317

read = rbind(7,8,5,6,7,4,6,5,6,4,3,5,4,7,6)
dance = rbind(10,9,10,10,8,4,4,5,6,5,1,3,2,1,3)
tv = rbind(6,5,5,6,7,4,5,5,6,6,1,1,2,2,3)
ski= rbind(5,7,8,8,9,4,3,6,7,5,2,5,5,4,3)
names = rep(rbind("belly","politic","admin"),c(5,5,5))

multdv = data.frame(names,read,dance,tv,ski)
multdv


# Plot the group means

install.packages("ggplot2")
install.packages("reshape")
library(ggplot2)
library(reshape)

Group = matrix(rep(cbind("belly","politic","admin"),c(4)))
Variable = matrix(rep(cbind("read","dance","tv","ski"),c(3,3,3,3)))
Avg = matrix(cbind(6.6,5.0,5.0,9.4,4.8,2.0,5.8,5.2,1.8,7.4,5.0,3.8))
newfile = data.frame(Group,Variable,Avg)

#  No need to reshape newfile
#ppfile = melt(multdv, id = "names", measured = c("read","dance","tv","ski"))
#names = factor(names)
#ppfile

line = ggplot(newfile,aes(x=Variable,y=Avg,group=Group))
line + geom_line(aes(linetype=Group)) + labs(list(title="Mean Leisure Activity",x="Dependent Variables",y="Mean Leisure"))


###############################################################
# MEANS FOR Avg matrix above
###############################################################

# Compute means for the three groups across dependent variables

belly = (multdv[1:5,])
bellymn = c(mean(belly$read),mean(belly$dance),mean(belly$tv),mean(belly$ski)) 
bellymn

politic = (multdv[6:10,])
politicmn = c(mean(politic$read),mean(politic$dance),mean(politic$tv),mean(politic$ski)) 
politicmn

admin = (multdv[11:15,])
adminmn = c(mean(admin$read),mean(admin$dance),mean(admin$tv),mean(admin$ski)) 
adminmn

###############################
Difference Scores
##############################

attach(multdv)
RD = read - dance
DT = dance - tv
TS = tv - ski
diff = data.frame(names,RD,DT,TS)
diff

##################################
#one-way MANOVA
##################################

library(MASS)
options(scipen=999)
outcome = cbind(RD,DT,TS)
names = factor(names)
model = manova(outcome ~ names,data = diff)

#############################
Multivariate results
############################

summary(model,test="Wilks")
summary(model,test="Pillai")
summary(model,test="Hotelling")
summary(model,test="Roy")
 



