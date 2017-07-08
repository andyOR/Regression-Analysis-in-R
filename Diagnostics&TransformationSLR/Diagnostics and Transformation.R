# Reading the data and visualizing the linear fit

anscombe <- read.table("Data/anscombe.txt", header = TRUE)
attach(anscombe)
par(mfrow = c(2, 2))
# Linear relationship
plot(x1, y1, xlim = c(4, 20), ylim = c(3, 14), main = "Data Set 1")
abline(lsfit(x1, y1))
# presence of non-linear term
plot(x2, y2, xlim = c(4, 20), ylim = c(3, 14), main = "Data Set 2")
abline(lsfit(x2, y2))
# Presence of outlier
plot(x3, y3, xlim = c(4, 20), ylim = c(3, 14), main = "Data Set 3")
abline(lsfit(x3, y3))
# No linear relationship
plot(x4, y4, xlim = c(4, 20), ylim = c(3, 14), main = "Data Set 4")
abline(lsfit(x4, y4))

## A plot of residuals against X that produces a random pattern indicates an appropriate model has been fit to the data.
## A plot of residuals against X that produces a discernible pattern indicates an incorrect model has been fit to the data


## Residuals plots let us to assess visually whether an appropriate model has been fit to the data no matter how many predictor variables are used

par(mfrow = c(2, 2))
m1 <- lm(y1 ~ x1, data = anscombe)
plot(anscombe$x1, m1$residuals, ylab = "Residuals", xlim = c(4, 20), ylim = c(-3.5,
                                                                              3.5), main = "Data Set 1")
m2 <- lm(y2 ~ x2, data = anscombe)
plot(anscombe$x2, m2$residuals, ylab = "Residuals", xlim = c(4, 20), ylim = c(-3.5,
                                                                              3.5), main = "Data Set 2")
m3 <- lm(y3 ~ x3, data = anscombe)
plot(anscombe$x3, m3$residuals, ylab = "Residuals", xlim = c(4, 20), ylim = c(-3.5,
                                                                              3.5), main = "Data Set 3")
m4 <- lm(y4 ~ x4, data = anscombe)
plot(anscombe$x4, m4$residuals, ylab = "Residuals", xlim = c(4, 20), ylim = c(-3.5,
                                                                              3.5), main = "Data Set 4")

## Leverage Points: Leverage points are those which have great influence on the fitted model, i.e., whose x-value is distant from the other x-values
# A popular rule to classify Xias a point of high leverage YJ in a SLR model if h > 4/n
huber <- read.table("huber.txt", header = TRUE)
mBad <- lm(YBad ~ x, data = huber)
mGood <- lm(YGood ~ x, data = huber)
par(mfrow = c(1, 2))
plot(hatvalues(mBad), main = "Hat Values for YBad")
abline(h = 4/6, lty = 2, col = 2)
plot(hatvalues(mGood), main = "Hat Values for YGood")
abline(h = 4/6, lty = 2, col = 2)

# A bad leverage point is a leverage point which is also an outlier.
# A point is a bad leverage point if its -value does not follow the pattern set by the other data points

## Dealing with Bad Leverage points
# Remove invalid data points and refit the model without them
# Fit a different regression model- Higher-order terms, Transformation


## R-squared
# R-squared (coefficient of determination) is a measure of how close the data are to the fitted regression line.
# R-squared = SSreg/ SST : Explained variation/Total variation

set.seed(456)
n = 1000
x = rnorm(n)
y1 <- 1 + 2 * x + rnorm(n)
y2 <- 1 + 2 * x + rnorm(n, 0, 3)
par(mfrow = c(1, 2))
plot(x, y1)
abline(lsfit(x, y1))
plot(x, y2)
abline(lsfit(x, y2))

## Summary statitics - Rsquared for plot 1
summary(lm(y1 ~ x))

# Call:
#   lm(formula = y1 ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.87917 -0.66129 -0.03604  0.69126  2.94421 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.06259    0.03181   33.41   <2e-16 ***
#   x            2.00506    0.03239   61.90   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.004 on 998 degrees of freedom
# Multiple R-squared:  0.7934,	Adjusted R-squared:  0.7931 
# F-statistic:  3831 on 1 and 998 DF,  p-value: < 2.2e-16


## Summary statistics - Rsquared for plot 2
summary(lm(y2 ~ x))

# Call:
#   lm(formula = y2 ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.1712  -1.9696   0.1058   1.9246  11.5658 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.88035    0.09161   9.609   <2e-16 ***
#   x            2.04892    0.09330  21.961   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.892 on 998 degrees of freedom
# Multiple R-squared:  0.3258,	Adjusted R-squared:  0.3251 
# F-statistic: 482.3 on 1 and 998 DF,  p-value: < 2.2e-16

## R=sqaured does not indicate whether a regression model is adequate. You can have a low value R-squared for a good model, or a high value R-squared for a model that does not fit the data
## Use F-test for relationship


