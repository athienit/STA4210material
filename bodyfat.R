##### Bodyfat example ###
dat=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%207%20Data%20Sets/CH07TA01.txt",
               col.names=c("X1","X2","X3","Y"))

### Choosing model using sequential sums of squares 
reg123=lm(Y~X1+X2+X3,data=dat)
summary(reg123)
anova(reg123)

reg213=lm(Y~X2+X1+X3,data=dat)
anova(reg213)

reg321=lm(Y~X3+X2+X1,data=dat)
anova(reg321)

### Model with only x2
reg2=update(reg123,.~.-X1-X3)
summary(reg2)

reg132=lm(Y~X1+X3+X2,data=dat)
anova(reg132)



### Simultaneous test, can we remove x1 and x3 simultaneously H0:beta1=beta3=0
anova(reg2,reg123)

### How about model with only x2 removed
reg13=update(reg123,.~.-X2)
summary(reg13)
anova(reg13)


### Equivalency of t-test to F-test for only 1 coefficient (using type II SS),
# i.e. SS(x1|x2,x3)
#      SS(x2|x1,x3)
#      SS(x3|x1,x2)
summary(reg123)
library(car)
SS2=Anova(reg123,type=2);SS2 #notice same p-values

### Test whether H0:beta2=1/2*beta1+1/2*beta3 (not necessarily equal to zero)
dat[,"Z1"]=dat[,"X1"]+1/2*dat[,"X2"]
dat[,"Z2"]=dat[,"X3"]+1/2*dat[,"X2"]
reg2eq13=lm(Y~Z1+Z2,data=dat)
anova(reg2eq13,reg123)

reg13=update(reg123,.~.-X2)
summary(reg13)

### Coefficient of partial determination R^2_{Y x2|x1 x3}=SSR(x2|x1 x3)/SSE(x1 x3)
SST=(dim(dat)[1]-1)*var(dat$Y)
SS2["X2","Sum Sq"]/anova(lm(Y~X1+X3,data=dat))["Residuals","Sum Sq"]

### Coefficient of partial determination R^2_{Y x1 x3|x2} = SSR(x1 x3|x2)/SSE(x2) = [SSE(x2)-SSE(x1 x2 x3)]/SSE(x2)
SSEx2=anova(lm(Y~X2,data=dat))["Residuals","Sum Sq"]
(SSEx2-anova(reg123)["Residuals","Sum Sq"])/SSEx2

#### Standardized regression ####
round(cor(dat[,1:4]),2)
library(car)
sqrt(vif(reg13))

cor.trans=function(y){
  n=length(y)
  1/sqrt(n-1)*(y-mean(y))/sd(y)
}
dat_trans=as.data.frame(apply(dat[,1:4],2,cor.trans))
reg13_trans=lm(Y~0+X1+X3,data=dat_trans)
summary(reg13_trans)

sqrt(vif(reg123))
