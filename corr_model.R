muscle=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR27.txt",
           col.names=c("Y1","Y2"))

### Model Y1|Y2
#reg=lm(Y1~Y2,data=muscle)
#summary(reg)$coefficients[,1]

# estimate parameters manually
attach(muscle)
n=length(Y1)
r=cor(Y1,Y2);r
b1=r*sd(Y1)/sd(Y2);b1
b0=mean(Y1)-mean(Y2)*b1;b0
s=var(Y1)*(1-r^2);s

# Test H0:rho=0
TS=(r*sqrt(n-2))/sqrt(1-r^2)
2*pt(-abs(TS),n-2) #2 sided pvalue

# 95% CI for rho
zp=0.5*log((1+r)/(1-r))
LU=zp+c(1,-1)*qnorm(0.025)*1/sqrt(n-3)
(exp(2*LU)-1)/(exp(2*LU)+1)

# What if we cannot assume normality?
rs=cor(Y1,Y2,method="spearman");rs # default method is pearson
TSs=(rs*sqrt(n-2))/sqrt(1-rs^2)
2*pt(-abs(TSs),n-2) #2 sided pvalue
