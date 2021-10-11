#Flyash % as a strength (sten) factor in concrete compression test (PSI) for 28 day cured concrete
dat=matrix(c(0, 4779,
             0, 4706,
             0, 4350,
             20, 5189,
             20, 5140,
             20, 4976,
             30, 5110,
             30, 5685,
             30, 5618,
             40, 5995,
             40, 5628,
             40, 5897,
             50, 5746,
             50, 5719,
             50, 5782,
             60, 4895,
             60, 5030,
             60, 4648),
           18,2,byrow=TRUE,dimnames=list(c(),c("flyash","strength")))

plot(dat)
dat=as.data.frame(dat)
reg.lin=lm(strength~flyash,data=dat)
summary(reg.lin)
abline(reg.lin)

#Add 2nd order polynomial
flyash2=dat$flyash^2
reg.2poly=lm(strength~flyash+flyash2,data=dat)
summary(reg.2poly)
curve(reg.2poly$coefficients[1]+reg.2poly$coefficients[2]*x+reg.2poly$coefficients[3]*x^2,from=0,to=60,col=2,add=TRUE)

#Add 3rd order 
flyash3=dat$flyash^3
reg.3poly=lm(strength~flyash+flyash2+flyash3,data=dat)
summary(reg.3poly)

reg.3polym1=update(reg.3poly,.~.-flyash)
summary(reg.3polym1)
curve(reg.3polym1$coefficients[1]+reg.3polym1$coefficients[2]*x^2
      +reg.3polym1$coefficients[3]*x^3,from=0,to=60,col=3,add=TRUE)
legend(0,6000,legend=c("1st order","2nd order","3rd order"),lty=1,col=1:3,bg="light gray")
