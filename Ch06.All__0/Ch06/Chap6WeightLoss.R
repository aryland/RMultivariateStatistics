# Chapter 6 
# Doubly Multivariate Repeated Measures 
# Weight Loss Example
# 34 subjects on 7 variables
#  group:  control, diet, dietex
#  w11, w12, w13 : weight loss at 1, 2, and 3 months
#  se1, se2, se3 : self esteem at 1, 2, and 3 months
# Include Helmert contrasts for group
# Include Helmert contrast for control vs (diet + dietex)
# Include Helmert ocntrast for diet vs dietex

library(car)
attach(WeightLoss)

## A "doubly multivariate" design with two  distinct repeated-measures variables
## (example courtesy of Michael Friendly)
## See ?WeightLoss for a description of the dataset.
 
imatrix <- matrix(c(
    1,0,-1, 1, 0, 0,
    1,0, 0,-2, 0, 0,
    1,0, 1, 1, 0, 0,
    0,1, 0, 0,-1, 1,
    0,1, 0, 0, 0,-2,
    0,1, 0, 0, 1, 1), 6, 6, byrow=TRUE)
colnames(imatrix) <- c("WL", "SE", "WL.L", "WL.Q", "SE.L", "SE.Q")
rownames(imatrix) <- colnames(WeightLoss)[-1]
(imatrix <- list(measure=imatrix[,1:2], month=imatrix[,3:6]))

# Helmert contrasts vs (diet + dietex) and diet vs dietex
contrasts(WeightLoss$group) <- matrix(c(-2,1,1, 0,-1,1), ncol=2) 

(wl.mod<-manova(cbind(wl1, wl2, wl3, se1, se2, se3)~group, data=WeightLoss))

Anova(wl.mod, imatrix=imatrix, test="Roy",type=3)

(wl.mod<-lm(cbind(wl1, wl2, wl3, se1, se2, se3)~group, data=WeightLoss))

Anova(wl.mod, imatrix=imatrix, test="Roy",type=3)

