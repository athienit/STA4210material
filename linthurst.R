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
anova(linthurst.model)

# correlation matrix
round(cor(linthurst[c("biomass","salinity","pH","K","Na","Zn")]),3)  
# correlation between (Zn and pH) and (Na and K)

# Remove K
modu1<- update(linthurst.model, . ~ . -K)
summary(modu1)

# Remove Zn
modu2<- update(linthurst.model, . ~ . -K-Zn)
summary(modu2)

# Remove salinity
modu3<- update(linthurst.model, . ~ . -K-Zn-salinity)
summary(modu3)

# Remove Na
modu4<- update(linthurst.model, . ~ . -K-Zn-salinity-Na)
summary(modu4)

# Fit biomass for pH=4.15 AND Na=10000. and create a P.I.
newdata=data.frame(pH=4.15,Na=10000)
predict(modu3, newdata, interval="prediction",level=0.95)

# Next one should check the assumptions
source("http://www.stat.ufl.edu/~athienit/check.R")
check(modu3,tests=TRUE)