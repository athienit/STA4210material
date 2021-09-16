# find relationship between lot size (X) and work hours (Y) 
toluca=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01TA01.txt",
                  col.names = c("lotsize", "workhrs"))
plot(toluca$lotsize,toluca$workhrs)
toluca.reg=lm(workhrs~lotsize,data=toluca)
summary(toluca.reg)
abline(toluca.reg,col="darkgreen")

source("http://www.stat.ufl.edu/~athienit/check.R")
check(toluca.reg,tests=TRUE)

mycount=seq(20,120,by=1)
fitl=loess(workhrs~lotsize,span=0.5,data=toluca)
predl=predict(fitl,mycount,se=TRUE)

plot(toluca)
lines(mycount,predl$fit,lty=1,col="darkred")
lines(mycount,predl$fit-1.96*predl$se.fit, lty=2, col="blue", lwd=1)
lines(mycount,predl$fit+1.96*predl$se.fit, lty=2, col="blue", lwd=1)

polygon(c(mycount,rev(mycount)),c(predl$fit+1.96*predl$se.fit,rev(predl$fit-1.96*predl$se.fit)),col="#00009933",border="NA")

abline(toluca.reg,col="darkgreen")
legend("topleft",legend=c(expression(paste("Loess  ",alpha,"=0.5")),"95% CB","SLR"),
       col=c("darkred","blue","darkgreen"),lty=c(5,2,1),bg="gray90")
