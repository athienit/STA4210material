# find relationship between lot size (X) and work hours (Y) 
toluca=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01TA01.txt",
                  col.names = c("lotsize", "workhrs"))
plot(toluca$lotsize,toluca$workhrs)
toluca.reg=lm(workhrs~lotsize,data=toluca)
summary(toluca.reg)
abline(toluca.reg)

fitted(toluca.reg)
round(resid(toluca.reg),1)
round(rstandard(toluca.reg),1)

#lets estimate workhours for lotsize=75
predict(toluca.reg,newdata=data.frame(lotsize=75))

### Chapter 2 (inference) ###
# inference on coefficients
summary(toluca.reg)$coefficients
confint(toluca.reg,type="wald",level=0.95)

# CI on mean response when lotsize=75
predict(toluca.reg,newdata=data.frame(lotsize=75),se.fit=TRUE,interval="confidence")

# PI on predicted value when lotsize=75
predict(toluca.reg,newdata=data.frame(lotsize=75),interval="prediction")

# Working-Hotelling Confidence band
CI=predict(toluca.reg,se.fit=TRUE)
W=sqrt(2*qf(0.95,length(toluca.reg$coefficients),toluca.reg$df.residual))
Band=cbind( CI$fit - W * CI$se.fit, CI$fit + W * CI$se.fit )

points(sort(toluca$lotsize), sort(Band[,1]), type="l", lty=2)
points(sort(toluca$lotsize), sort(Band[,2]), type="l", lty=2)
legend("topleft",legend=c("Mean Line","95% CB"),col=c(1,1),lty=c(1,2),bg="gray90")

# However, if your estimated regression line has a negative slope, 
# you need to sort the columns of Band in reverse order.  So change the above R commands to:
# points(sort(Data$ACT), sort(Band[,1], decreasing=TRUE),type="l", lty=2)
# points(sort(Data$ACT), sort(Band[,2], decreasing=TRUE),type="l", lty=2)

### ANOVA table
anova(toluca.reg)
summary(toluca.reg)

### Correlation inference
r=cor(toluca$workhrs,toluca$lotsize);r
zp=0.5*log((1+r)/(1-r));zp #Fisher transformation
zpCI=zp+c(1,-1)*qnorm(0.025)*sqrt(1/(25-3));zpCI #CI for Fisher's

(exp(2*zpCI)-1)/(exp(2*zpCI)+1) #Transform Fisher's CI to correlation coefficient CI
