heart=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2011%20Data%20Sets/CH11TA01.txt",
                 col.names=c("age","pressure"))
reg=lm(pressure~age,data=heart)
summary(reg)

# Check model assumptions
par(mfrow=c(2,2))
plot(reg)

#Do Ch10 diagnostics
infl=influence.measures(reg);infl

### IRLS/IWLS
# Method 1
library(MASS)
reg.iwls=rlm(pressure~age,data=heart,wt.method="inv.var",method="MM")
summary(reg.iwls)

# Method 2
glm.reg=glm(pressure~age,data=heart,method="glm.fit")
summary(reg.iwls)

#####################################
##### ONE ITERATION MANUALLY of IRLS
#####################################
##(1) obtain residuals from OLS
e=residuals(reg)
##(2) obtain robust residuals
u=e/mad(e)
##(3) use huber weight function on residuals
w=psi.huber(u)
##(4) fit WLS
#(a) automatically, obtain residuals from this model and repeat
reg_v2=lm(formula(reg),weights=w,data=heart)

#(b) manually
X=model.matrix(reg)
y=heart$pressure
W=diag(w)
H=X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%W
ev2=(diag(length(y))-H)%*%y #recall e=(I-H)y

#lets check if (a)=(b) to 4 decimal places
round(ev2,4)==round(resid(reg_v2),4)
