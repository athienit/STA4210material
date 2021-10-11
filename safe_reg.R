dat=read.csv("http://www.stat.ufl.edu/~athienit/STA4210/Examples/safe_reg.csv",header=TRUE)
# response is "lost hours"
# predictor1 is "number of employess"
# predictor2 is 1 for safety program, 0 otherwise

attach(dat)

library(car)
scatterplot(y~x1|x2,smooth=FALSE,reg.line=FALSE,data=dat)
reg1=lm(y~x1+x2)
coef1=coef(reg1)
curve(coef1[1]+coef1[3]+coef1[2]*x,from=1500,to=10000,add=T,col="red")
curve(coef1[1]+coef1[2]*x,from=1500,to=10000,add=T)

scatterplot(y~x1|x2,smooth=FALSE,reg.line=lm,data=dat)

reg2=lm(y~x1+x2+x1:x2) #same as lm(y~x1+x2+x1*x2) or lm(y~x1*x2)
summary(reg2)

scatterplot(y~x1|x2,smooth=FALSE,reg.line=lm,data=dat)

X=model.matrix(reg2);X # Matrix X
H= X %*% solve( t(X) %*% X ) %*% t(X);H
I=diag(40)
J=matrix(1,40,40)

# Estimate and predict at Xh=c(1,6500,0,0)
xh=c(1,6500,0,0)
t(reg2$coefficients)%*%xh #estimate
sqrt(315*t(xh)%*%solve(t(X)%*%X)%*%xh) #standard error OR sqrt(t(xh)%*%vcov(reg2)%*%xh)

predict(reg2,newdata=data.frame(x1=6500,x2=0),se.fit=TRUE,interval="confidence")

(which(xh%in%X)) # to detemine if xh matches any row in X, i.e. is in the data.
predict(reg2,newdata=data.frame(x1=6500,x2=0),se.fit=TRUE,interval="prediction")

# Goal is to create CI for beta1+beta3, to determine if slope is significant under safety program
vc=vcov(reg2);vc
sum(reg2$coefficients[c(2,4)])+c(1,-1)*qt(0.025,reg2$df.residual)*sqrt(vc[2,2]+vc[4,4]+2*vc[2,4])
