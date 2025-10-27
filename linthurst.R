#Linthurst Data: Spartina Biomass

linthurst <- read.table("https://raw.githubusercontent.com/athienit/STA4210material/refs/heads/main/linthurst.txt",
                        row.names=1,  # interpret column 1 as row names
                        skip=1,
                        col.names=c("obsnum","loc","type","biomass",
                                    "salinity","pH","K","Na","Zn"))
print(linthurst)

# regression model
linthurst.model <- lm(biomass ~ salinity + pH + K + Na + Zn, data=linthurst)
summary(linthurst.model)

# correlation matrix
round(cor(linthurst[c("biomass","salinity","pH","K","Na","Zn")]),3)  
# correlation between (Zn and pH) and (Na and K)

# Remove Na
modu1<- update(linthurst.model, . ~ . -Na)
summary(modu1)

# Remove Na and salinity
modu2<- update(linthurst.model, . ~ . -Na-salinity)
summary(modu2)

# Remove Na and salinity and Zn
modu3<- update(linthurst.model, . ~ . -Na-salinity-Zn)
summary(modu3)
AIC(modu3)

# Remove K too?
modu4<- update(linthurst.model, . ~ . -Na-salinity-Zn-K)
summary(modu4)

# Final model with Na instead of K?
modu5<-update(modu3, .~.-K+Na)
summary(modu5)
AIC(modu5)

# simultaneous full vs reduced model
anova(modu5,linthurst.model)

# Fit biomass for pH=4.15 AND Na=10000. and create a P.I.
newdata=data.frame(pH=4.15,Na=10000)
predict(modu5, newdata, interval="prediction",level=0.95)

# Next one should check the assumptions
source("http://www.stat.ufl.edu/~athienit/check.R")
check(modu3,tests=TRUE)