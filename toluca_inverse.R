# find relationship between lot size (X) and work hours (Y) 
toluca=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01TA01.txt",
                  col.names = c("lotsize", "workhrs"))
plot(toluca$lotsize,toluca$workhrs)
toluca.reg=lm(workhrs~lotsize,data=toluca)
summary(toluca.reg)
abline(toluca.reg)
anova(toluca.reg)

MSE=anova(toluca.reg)["Residuals","Mean Sq"]
b0=as.numeric(coefficients(toluca.reg)[1])
b1=as.numeric(coefficients(toluca.reg)[2])
SSx=(25-1)*var(toluca$lotsize) # Sum Squares x's
mx=mean(toluca$lotsize)

# check condition for 95%
(qt(1-0.05/2,23)^2*MSE)/(b1^2*SSx) <0.1

# Take y_new = 420
xnew=(420-b0)/b1;xnew

xnew+c(-1,1)*qt(1-0.05/2,23)*sqrt(MSE/b1^2*(1+1/25+(xnew-mx)^2/SSx))
