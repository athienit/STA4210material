# Easy-to-interpret R script: one predictor, one response (positive correlation + wiggles)
# Fits: high-degree OLS (overfit), ridge (alpha=0), lasso (alpha=1), elastic-net (alpha=0.5)
# Plots: scatter + true function + fitted curves
set.seed(123)

# Packages
library(glmnet)
library(ggplot2)

# Correlation-transformation helper (center & scale to unit SD)
corr_transform <- function(v) as.numeric((1/(length(v)-1))*(v - mean(v)) / sd(v))

# 1) Simulate data (positive linear trend + three "wiggles")
n <- 100
x_raw <- runif(n, 0, 10)
true_fun <- function(x) 2 + 0.6 * x + 1.5 * sin(1.2 * x) + 0.8 * sin(0.35 * x) + 0.6 * cos(0.75 * x)
y_raw <- true_fun(x_raw) + rnorm(n, sd = 2)   # response in original scale
df <- data.frame(x_raw = x_raw, y_raw = y_raw)

# Basic scatterplot of the raw data (base R) - Step 1
plot(df$x_raw, df$y_raw, pch = 16, col = "grey30",
     main = "Step 1: Raw data (scatter)",
     xlab = "x (raw)", ylab = "y (raw)")

# 2) Standardize predictor using correlation transform (center & scale)
x <- corr_transform(x_raw)    # standardized predictor
y <- corr_transform(y_raw)    # standardized response
df$x <- x
df$y <- y

# Fit a high-degree polynomial OLS (encourage overfitting)
# unstandardized
lm_poly_raw <- lm(y_raw ~ poly(x_raw, deg=10, raw = TRUE), data = df)
summary(lm_poly_raw)

########  READ ###############
# raw high‑degree polynomials can produce very large or very small numbers 
# (see the enormous coefficient magnitudes in the standardized-output summary), 
# which worsens numerical precision and makes SEs unreliable.
###############################

# standardized
deg <- 10
lm_poly <- lm(y ~ poly(x, deg, raw = TRUE), data = df)
summary(lm_poly)

# Plot OLS fit 
xg <- seq(min(x), max(x), length.out = 300)
pred_lm <- predict(lm_poly, newdata = data.frame(x = xg))

plot(df$x, df$y, pch = 16, col = "grey30",
     main = "high-degree OLS fit (standardized)",
     xlab = "x (std)", ylab = "y (std)")
lines(xg, pred_lm, col = "#D55E00", lwd = 2)
legend("bottomright",
       legend = c("Data", "High-degree OLS"),
       pch = c(16, NA),
       lty = c(NA, 1),
       col = c("grey30", "#D55E00"),
       bty = "o", # draw box
       bg = "lightgray", # background color of the box
       box.col = adjustcolor("darkgray", alpha.f = 0.6),
       cex = 0.9)

# 3) # Use raw x/y and let glmnet standardize (default)
X <- model.matrix(~ poly(x_raw, deg, raw = TRUE), data = df)[ , -1]  # drop intercept col for glmnet
y_vec <- df$y_raw

# Fit regularized models (cv.glmnet) with standardize = FALSE
cv_ridge <- cv.glmnet(X, y_vec, alpha = 0, nfolds = 10, type.measure = "mse",family="gaussian")
cv_ridge
# Note default family is Gaussian and for Gaussian, default measure is MSE
cv_lasso <- cv.glmnet(X, y_vec, alpha = 1, nfolds = 10)
cv_lasso

########  READ  ###########
# lambda.min is the value of lambda that gives the smallest cross‑validated error; 
# lambda.1se is the largest (i.e., most regularized / simpler) lambda
# whose mean CV error is within one standard error of the minimum. 
# The 1‑se rule favors a simpler, more stable model with nearly the same 
# CV performance as the best (min) model.
###########################

# CV to find alpha for elastic net
train_rows <- sample(1:n, .66*n)
x.train <- X[train_rows, ]
x.test <- X[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

alphas <- seq(0, 1, by = 0.1)
list.of.fits <- vector("list", length(alphas))
names(list.of.fits) <- as.character(alphas)

# fit CV for each alpha (glmnet will standardize columns by default)
for(i in seq_along(alphas)) {
  a <- alphas[i]
  list.of.fits[[i]] <- cv.glmnet(x.train, y.train, alpha = a, nfolds = 10)
}

# evaluate on test set using lambda.1se and lambda.min
results <- data.frame(alpha = alphas,
                      mse_1se = NA_real_,
                      mse_min = NA_real_)

for(i in seq_along(alphas)) {
  fit <- list.of.fits[[i]]
  pred_1se <- predict(fit, newx = x.test, s = "lambda.1se")
  pred_min <- predict(fit, newx = x.test, s = "lambda.min")
  results$mse_1se[i] <- mean((y.test - pred_1se)^2)
  results$mse_min[i] <- mean((y.test - pred_min)^2)
}

print(results)        # compare performance across alphas

##########  READ ############
# two different comparisons because mse_1se and mse_min were computed from different models: 
# mse_1se uses the model fit at each fit’s lambda.1se (the simpler, more regularized choice), 
# mse_min uses the model fit at lambda.min (with smallest CV error). 
# Those are different lambda values (and thus different fitted models) for each alpha, so
# the best alpha can differ depending on which rule you evaluate. 
# Differences are also very small, so sampling/noise can easily change the winner.
#############################

# choose best alpha (example: by mse_1se)
best_idx <- which.min(results$mse_1se)
best_alpha <- results$alpha[best_idx]
best_fit <- list.of.fits[[best_idx]]
best_lambda <- best_fit$lambda.1se

cat("Best alpha (by test MSE at lambda.1se):", best_alpha,
    "with lambda.1se =", round(best_lambda, 5), "\n")

# inspect coefficients at chosen alpha/lambda
print(coef(best_fit, s = "lambda.1se"))

# (Optional) Refit final model on the full dataset using chosen alpha & lambda
# x_full and y_full should be the model.matrix and response using all available data
final_fit_ridge <- glmnet(X, y_raw, alpha = 0, lambda = cv_ridge$lambda.1se)
print(coef(final_fit_ridge))
final_fit_lasso <- glmnet(X, y_raw, alpha = 1, lambda = cv_lasso$lambda.1se)
print(coef(final_fit_lasso))
final_fit_enet <- glmnet(X, y_raw, alpha = best_alpha, lambda = best_lambda)
print(coef(final_fit_enet))

# Prediction grid on raw scale and corresponding design matrix
xg_raw <- seq(min(x_raw), max(x_raw), length.out = 300)
Xg <- model.matrix(~ poly(xg_raw, deg, raw = TRUE), data = data.frame(xg_raw = xg_raw))[ , -1]

# Predictions on grid (use design matrix built from standardized xg)
pred_ridge <- as.numeric(predict(final_fit_ridge, newx = Xg, s = "lambda.1se"))
pred_lasso <- as.numeric(predict(final_fit_lasso, newx = Xg, s = "lambda.1se"))
pred_enet  <- as.numeric(predict(final_fit_enet,  newx = Xg, s = "lambda.1se"))

# Base R plot overlaying OLS and the three regularized fits (Step 3)
plot(df$x_raw, df$y_raw, pch = 16, col = "grey30",
     main = "Step 3: OLS vs Ridge / Lasso / Elastic-net (base R)",
     xlab = "x (raw)", ylab = "y")
lines(xg_raw, pred_lm_raw,    col = "#D55E00", lwd = 2)  # OLS (overfit)
lines(xg_raw, pred_ridge, col = "#0072B2", lwd = 2)  # Ridge
lines(xg_raw, pred_lasso, col = "#009E73", lwd = 2)  # Lasso
lines(xg_raw, pred_enet,  col = "#CC79A7", lwd = 2)  # Elastic-net
legend("topleft", legend = c("Data","OLS (deg 10)","Ridge","Lasso","Elastic-net"),
       col = c("grey30","#D55E00","#0072B2","#009E73","#CC79A7"),lwd=2,
       pch = c(16, NA, NA, NA, NA), lty = c(NA,1,1,1,1), bty = "n")

# 4) Fancy ggplot comparison (keeps your original ggplot approach) - final compare
plot_df <- data.frame(
  x = xg_raw,
  true = true_fun(xg_raw),
  lm = pred_lm_raw,
  ridge = pred_ridge,
  lasso = pred_lasso,
  enet = pred_enet
)

p <- ggplot() +
  # map data points to a label so they appear in the legend
  geom_point(data = df, aes(x = x_raw, y = y_raw, color = "Data"), alpha = 0.7) +
  # map each line to a label (string inside aes -> shows in legend)
  geom_line(data = plot_df, aes(x = x, y = true, color = "True"), size = 1, linetype = "dashed") +
  geom_line(data = plot_df, aes(x = x, y = lm, color = "OLS"), size = 0.9) +
  geom_line(data = plot_df, aes(x = x, y = ridge, color = "Ridge"), size = 0.9) +
  geom_line(data = plot_df, aes(x = x, y = lasso, color = "Lasso"), size = 0.9) +
  geom_line(data = plot_df, aes(x = x, y = enet, color = "Elastic-net"), size = 0.9) +
  # define the exact colors and the legend title
  scale_color_manual(name = "Model",
                     values = c(
                       "Data" = "grey30",
                       "True" = "black",
                       "OLS" = "#D55E00",
                       "Ridge" = "#0072B2",
                       "Lasso" = "#009E73",
                       "Elastic-net" = "#CC79A7"
                     )) +
  labs(title = "OLS (high-degree) vs Ridge, Lasso, Elastic-net",
       subtitle = "Points = data, dashed = true function; colored curves = fitted models",
       y = "y", x = "x") +
  theme_minimal() +
  # make legend lines/points display nicely
  guides(color = guide_legend(override.aes = list(
    linetype = c("blank", "solid", "solid", "solid", "solid", "dashed"),
    shape    = c(16, NA, NA, NA, NA, NA)
  )))
print(p)

