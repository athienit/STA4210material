cruise <- read.fwf("http://www.stat.ufl.edu/~winner/data/cruise_ship.dat", width=c(20,20,rep(8,7)),
                   col.names=c("ship", "cline", "age", "tonnage", "passengers", "length", "cabins", "passdens", "crew"))

attach(cruise)

fit3 <-  lm(crew~tonnage+passengers+length+cabins, data=cruise)
summary(fit3)
n=158

### Deleted residuals
# Obtain studentized deleted residuals
# manually
e3=resid(fit3)
t3=e3*(fit3$df.residual/(anova(fit3)["Residuals","Sum Sq"]*(1-hatvalues(fit3))-e3^2))^0.5
t3[1:6]
t3[which(abs(t3)>=abs(qt(0.05/n,fit3$df.residual)))]

# automatically, (also refer to rstandard)
sdr=rstudent(fit3)
sdr[which(abs(sdr)>=abs(qt(0.05/n,fit3$df.residual)))]

### Observations with x-values with the potential to "pull" the regression line
hat=hatvalues(fit3)
hat[which(hat>2*5/n)]

### DFFITS
dffits(fit3)[which(dffits(fit3)>2*sqrt(5/n))]

### Cooks D
cd=cooks.distance(fit3)
cd[which(cd>1)] # criterion >1
cd[which(cd>qf(0.5,5,df.residual(fit3)))] # more conservative

### DFBETAS
dfb=dfbetas(fit3)
head(dfb)
which(dfb>2/sqrt(n),arr.ind=T)

### VIF
library(car)
vif(fit3) # variance inflation factors 
max(vif(fit3))>5
mean(vif(fit3))
