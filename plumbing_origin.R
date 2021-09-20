plumbing=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%204%20Data%20Sets/CH04TA02.txt",
           col.names=c("work","labor"))

with(plumbing,plot(labor~work,pch=16))

plumb.reg=lm(labor~0+work,data=plumbing)
summary(plumb.reg)
abline(plumb.reg)

confint(plumb.reg,type="Wald")

#PI when x=100
syhat=sqrt(223.42*(100^2/sum(plumbing$work^2)));syhat
spred=sqrt(223.42*(1+100^2/sum(plumbing$work^2)));spred
predict.lm(plumb.reg,newdata=data.frame(work=100))+c(1,-1)*qt(0.025,11)*spred
predict.lm(plumb.reg,newdata=data.frame(work=100),se.fit=TRUE,interval="prediction")
#Note that the se.fit printed is for the se for yhat not pred.
