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
