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

######################
### Basic 3-D plot ###
######################
# fit the 2-predictor model (pH and Na)
mod_ph_Na <- lm(biomass ~ pH + Na, data = linthurst)
summary(mod_ph_Na)

# 3-D scatter + regression plane (static)
library(scatterplot3d)

s3d <- scatterplot3d(linthurst$pH, linthurst$Na, linthurst$biomass,
                     pch=16, color="black",
                     xlab="pH", ylab="Na", zlab="Biomass",
                     main="Biomass ~ pH + Na")
# add the fitted hyperplane
s3d$plane3d(mod_ph_Na, draw_polygon=TRUE,
            polygon_args=list(col=rgb(0,0.3,0.8,0.35)))

######################
### Fancy 3-D plot ###
######################
library(plotly)

# fit the two-predictor model
mod_ph_Na <- lm(biomass ~ pH + Na, data = linthurst)

# create a grid to evaluate the fitted plane
pH.seq <- seq(min(linthurst$pH), max(linthurst$pH), length.out = 40)
Na.seq <- seq(min(linthurst$Na), max(linthurst$Na), length.out = 40)
grid <- expand.grid(pH = pH.seq, Na = Na.seq)
grid$biomass <- predict(mod_ph_Na, newdata = grid)

# convert predicted values to a matrix for the surface
z.mat <- matrix(grid$biomass, nrow = length(pH.seq), ncol = length(Na.seq))

# interactive 3D scatter + regression surface
p <- plot_ly() %>%
  add_markers(data = linthurst,
              x = ~pH, y = ~Na, z = ~biomass,
              marker = list(size = 4, color = 'black'),
              hovertemplate = ~paste("obs:", rownames(linthurst),
                                     "<br>pH:", round(pH,3),
                                     "<br>Na:", round(Na,1),
                                     "<br>biomass:", round(biomass,3))) %>%
  add_surface(x = ~pH.seq, y = ~Na.seq, z = ~z.mat,
              showscale = FALSE, opacity = 0.45,
              surfacecolor = matrix(rep(mod_ph_Na$coefficients[1], length(pH.seq)*length(Na.seq)),
                                    nrow = length(pH.seq)),
              hoverinfo = "skip") %>%
  layout(scene = list(xaxis = list(title = "pH"),
                      yaxis = list(title = "Na"),
                      zaxis = list(title = "Biomass")),
         title = "Biomass ~ pH + Na (scatter + fitted plane)")

p
