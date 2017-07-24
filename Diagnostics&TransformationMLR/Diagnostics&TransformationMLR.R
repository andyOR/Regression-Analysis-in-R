## Added Variable Plots
# Added-variable plots (partial regression plots) enable us to visually assess the effect of
# each predictor, having adjusted for the effects of the other predictors
# Added variable plots is to isolate the effect of a particular Xj from  the rest of X's:

nyc <- read.csv("nyc.csv", header = T)
str(nyc)
# 'data.frame':	168 obs. of  7 variables:
#   $ Case      : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Restaurant: Factor w/ 168 levels "Amarone","Anche Vivolo",..: 47 149 18 20 46 89 90 119 16 37 ...
# $ Price     : int  43 32 34 41 54 52 34 34 39 44 ...
# $ Food      : int  22 20 21 20 24 22 22 20 22 21 ...
# $ Decor     : int  18 19 13 20 19 22 16 18 19 17 ...
# $ Service   : int  20 19 18 17 21 21 21 21 22 19 ...
# $ East      : int  0 0 0 0 0 0 0 1 1 1 ...

pairs(~Food + Decor + Service, data = nyc)

m1 <- lm(Price ~ Food + Decor + Service + East, nyc)
summary(m1)

# Call:
#   lm(formula = Price ~ Food + Decor + Service + East, data = nyc)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -14.0465  -3.8837   0.0373   3.3942  17.7491 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -24.023800   4.708359  -5.102 9.24e-07 ***
#   Food          1.538120   0.368951   4.169 4.96e-05 ***
#   Decor         1.910087   0.217005   8.802 1.87e-15 ***
#   Service      -0.002727   0.396232  -0.007   0.9945    
# East          2.068050   0.946739   2.184   0.0304 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.738 on 163 degrees of freedom
# Multiple R-squared:  0.6279,	Adjusted R-squared:  0.6187 
# F-statistic: 68.76 on 4 and 163 DF,  p-value: < 2.2e-16

par(mfrow = c(2, 2))
plot(nyc$Food, nyc$Price)
abline(lsfit(nyc$Food, nyc$Price))
plot(nyc$Decor, nyc$Price)
abline(lsfit(nyc$Decor, nyc$Price))
plot(nyc$Service, nyc$Price)
abline(lsfit(nyc$Service, nyc$Price))
plot(nyc$East, nyc$Price)
abline(lsfit(nyc$East, nyc$Price))

# Added variable plot
library(car)
avPlots(m1)

# From the added variable plots:
#   If it shows liner trend, it indicates a need for Xj in the model.
# If it shows curvature, it suggests a transformation of Xj is needed.
# Linear trend in the plot corresponds exactly to the t-test for partial coefficient Betaj.
# Added variable plots can help to spot outliers and leverage points.
# The plots are model-dependent: different models give different plots.


## Transformation
# Transformations in multiple regression can be used to
# Overcome problems due to nonlinearity
# Estimate percentage effects

# Inverse response plots
# Box-Cox procedure

# Transform only the response variable
defects <- read.table("defects.txt", header = TRUE)
str(defects)

# 'data.frame':	30 obs. of  5 variables:
#   $ Case       : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Temperature: num  0.97 2.85 2.95 2.84 1.84 2.05 1.5 2.48 2.23 3.02 ...
# $ Density    : num  32.1 21.1 20.6 22.5 27.4 ...
# $ Rate       : num  178 254 273 273 211 ...
# $ Defective  : num  0.2 47.9 50.9 49.7 11 15.6 5.5 37.4 27.8 58.7 ...

pairs(Defective ~ Temperature + Density + Rate, data = defects)
m1 <- lm(Defective ~ Temperature + Density + Rate, defects)
par(mfrow = c(2, 2))
StanRes1 <- rstandard(m1)
plot(defects$Temperature, StanRes1, ylab = "Standardized Residuals")
plot(defects$Density, StanRes1, ylab = "Standardized Residuals")
plot(defects$Rate, StanRes1, ylab = "Standardized Residuals")
plot(m1$fitted.values, StanRes1, ylab = "Standardized Residuals", xlab = "Fitted Values")

# The plots of standardized residuals against each predictor and the fitted values do
# not produce random scatters.

# Response against fitted value
fit1 <- m1$fitted.values
par(mfrow = c(1,1))
m2 <- lm(Defective ~ fit1 + I(fit1^2), defects)
plot(fit1, defects$Defective, xlab = "Fitted Values")
fitnew <- seq(-15, 60, len = 76)
lines(fitnew, predict(m2, newdata = data.frame(fit1 = fitnew)))
abline(lsfit(m1$fitted.values, defects$Defective), lty = 2)

# The summary plot of Y against Yhat shows a quadratic rather than a linear trend, i.e.,
# Y = g(Yhat) where is a quadratic function.
# It is natural to consider a transformation of Y

inverseResponsePlot(m1)
# lambda       RSS
# 1  0.4359947  541.9376
# 2 -1.0000000 6465.6551
# 3  0.0000000 1572.7444
# 4  1.0000000 1156.2704
# Rounding 0.44 to the nearest reasonable value, we shall transform Y by taking the square root

library(MASS)
boxcox(m1, lambda = seq(0.3, 0.65, length = 20))

#Transform Y
par(mfrow = c(2, 2))
plot(defects$Temperature, sqrt(defects$Defective), ylab = expression(sqrt(Defective)))
plot(defects$Density, sqrt(defects$Defective), ylab = expression(sqrt(Defective)))
plot(defects$Rate, sqrt(defects$Defective), ylab = expression(sqrt(Defective)))


##Residuals after transformation
mt <- lm(sqrt(Defective) ~ Temperature + Density + Rate, defects)
par(mfrow = c(2, 2))
StanRest <- rstandard(mt)
plot(defects$Temperature, StanRest, ylab = "Standardized Residuals")
plot(defects$Density, StanRest, ylab = "Standardized Residuals")
plot(defects$Rate, StanRest, ylab = "Standardized Residuals")
plot(mt$fitted.values, StanRest, ylab = "Standardized Residuals", xlab = "Fitted Values")
par(mfrow=c(1,1))
plot(mt$fitted.values, sqrt(defects$Defective), xlab = "Fitted Values", ylab = expression(sqrt(Defective)))
abline(lsfit(mt$fitted.values, sqrt(defects$Defective)))

## Diagnostics Plots
par(mfrow = c(2, 2))
plot(mt)

summary(mt)
# Call:
#   lm(formula = sqrt(Defective) ~ Temperature + Density + Rate, 
#      data = defects)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.10147 -0.28502 -0.07716  0.34139  1.13951 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  5.59297    5.26401   1.062   0.2978  
# Temperature  1.56516    0.66226   2.363   0.0259 *
#   Density     -0.29166    0.11954  -2.440   0.0218 *
#   Rate         0.01290    0.01043   1.237   0.2273  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5677 on 26 degrees of freedom
# Multiple R-squared:  0.943,	Adjusted R-squared:  0.9365 
# F-statistic: 143.5 on 3 and 26 DF,  p-value: 2.713e-16

## Added Variable Plots
avPlots(mt)


## END