cruise <- read.fwf("https://raw.githubusercontent.com/athienit/STA4210material/main/cruise_ship.dat.txt", width=c(20,20,rep(8,7)),
                   col.names=c("ship", "cline", "age", "tonnage", "passengers", "length", "cabins", "passdens", "crew"))

####### Fit Full model

fit0 <- lm(crew ~ age + tonnage + passengers + length + cabins + passdens,data=cruise)
summary(fit0)
AIC(fit0)
anova(fit0)

########## Perform all possible regressions (aka all subset regressions)
########## Prints out best 4 models of each # of predictors

library(leaps)
allcruise <- regsubsets(crew ~ age + tonnage + passengers + length + cabins + passdens,
                        nbest=4,data=cruise)
aprout <- summary(allcruise)
with(aprout,round(cbind(which,rsq,adjr2,cp,bic),3))     ## Prints "readable" results
plot(allcruise,scale="bic")
plot(allcruise,scale="adjr2")

fit3=update(fit0,.~.-age-passdens)
AIC(fit3)

##### Re-fit the "best" model from Stepwise Regression and calculate PRESS

library(qpcR)
PRESS(fit3)$stat
library(dbstats)
dblm(formula(fit3),method="OCV",data=cruise)$ocv
dblm(formula(fit3),method="OCV",data=cruise)$gcv

######### Perform Backward Elimination, Forward Selection using individual t tests
# Created function that takes a "full" regression model and 
# (backward) removes one term at a time if the individual t-test pvalue>alpha.rem
# OR
# (forward) adds one term at a time if the individual t-test pvalue<=alpha.enter
# OR 
# (both) performs forward until done and then checks existing model by performing backwards
source("https://raw.githubusercontent.com/athienit/STA4210material/main/stepT.R")
stepT(fit0,alpha.rem=0.2,direction="backward")
stepT(fit0,alpha.enter=0.2,direction="forward")
stepT(fit0,alpha.rem=0.2,alpha.enter=0.15,direction="both")

######### Perform Backward Elimination, Forward Selection, and Stepwise Regression
######### Based on Model AIC (not individual regression coefficients)
######### fit1 and fit2 represent "extreme" models

library(MASS)
fit1=lm(crew ~ age + tonnage + passengers + length + cabins + passdens, data=cruise)
fit2=lm(crew ~ 1, data=cruise)
stepAIC(fit1,direction="backward")
stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))
stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))


##### Cross-validation with a hold-out sample
##### Randomly sample 100 ships, fit model, obtain predictions for remaining 58 ships
#####    by applying their X-levels to regression coefficients from model

##### Obtain "training" and "validation" sets
# set random number seed for reproducible sampling based methods
set.seed(13579) 
cruise.cv.samp <- sample(1:length(cruise$crew),100,replace=FALSE)
cruise.cv.in <- cruise[cruise.cv.samp,]
cruise.cv.out <- cruise[-cruise.cv.samp,]

### Check if training sample (and validation) is similar to the whole dataset 
summary(cruise[,4:7])
summary(cruise.cv.in[,4:7])
summary(cruise.cv.out[,4:7])

##### Fit model for training set

fit.cv.in <- lm(crew ~tonnage + passengers + length + cabins,
                data=cruise.cv.in)
summary(fit.cv.in)
anova(fit.cv.in)

##### Obtain Predicted values and prediction errors for validation sample
##### Regression is based on same 4 predictors as fit3 (columns 4:7 of cruise)
##### Compute MSEP

pred.cv.out <- predict(fit.cv.in,cruise.cv.out[,4:7])
delta.cv.out <- cruise$crew[-cruise.cv.samp]-pred.cv.out
(mspe <- sum((delta.cv.out)^2)/length(cruise$crew[-cruise.cv.samp]))

# Calculate Mallow's Cp
anova(fit.cv.in)["Residuals","Sum Sq"]/anova(update(fit.cv.in,.~.+age+passdens))["Residuals","Mean Sq"]-(100-2*5)

