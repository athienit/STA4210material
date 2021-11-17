mp=read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2011%20Data%20Sets/CH11TA04.txt",
           col.names=c("State","MathP","Parents","HomeLib","Reading","TVWatch","Absences"))


reg=lm(MathP~Parents+HomeLib+Reading+TVWatch+Absences,data=mp)
plot(reg)
summary(reg)
round(cor(mp[2:7]),3)
# notice collinearity of Parents vs TvWatch, so remove Parents
# notice no correlation of Absences with response MathP, so remove Absences

reg2=update(reg,.~.-Parents-Absences)
anova(reg2,reg)

#Ch 10 diagnostics
infl=influence.measures(reg);infl

library(MASS)
reg.iwls=rlm(MathP~HomeLib+Reading+TVWatch,data=mp)
summary(reg.iwls)
cbind(reg.iwls$coefficients,reg$coefficients)
cbind(as.character(mp[which(reg.iwls$w<1),"State"]),round(reg.iwls$w[reg.iwls$w<1],4))
anova(reg.iwls)

# alternate method allows for more flexibility in specifying arguments...see ## Default S3 method in help(rlm)
#reg.iwls2=rlm(x=model.matrix(reg),y=mp[,"MathP"])
#summary(reg.iwls2)
#cbind(as.character(mp[which(reg.iwls2$w<1),"State"]),round(reg.iwls2$w[reg.iwls2$w<1],4))

