## Tranform both X and Y
# When some, or all, of the predictors and the response are highly skewed and
# transformations of these variables are desirable, there are 2 alternative approaches

# Approach 1 - transform x first and then find transfrom Y through inverse response plot
# Approach 2 - transform both X ad Y through BoxCox

#Approach 1
magazines <- read.csv("magazines.csv", header = TRUE)
str(magazines)

# 'data.frame':	204 obs. of  5 variables:
#   $ Magazine   : Factor w/ 204 levels "Advertising Age",..: 193 110 89 158 44 98 120 194 72 140 ...
# $ AdRevenue  : int  2280 3382 4218 4622 5121 5259 5838 6986 7634 8034 ...
# $ AdPages    : num  300 380 250 439 524 ...
# $ SubRevenue : int  854 968 2206 5555 4155 9048 4311 9202 2180 6846 ...
# $ NewsRevenue: int  16568 27215 12453 24282 9929 4363 10320 4048 63771 5271 ...

pairs(AdRevenue ~ AdPages + SubRevenue + NewsRevenue, magazines)
# BoxCox
# summary(a1 <- powerTransform(cbind(AdPages, SubRevenue, NewsRevenue) ~ -1, magazines))
# 
# bcPower Transformations to Multinormality 
# Est Power Rounded Pwr Wald Lwr bnd Wald Upr Bnd
# AdPages        0.1119        0.00      -0.0869       0.3107
# SubRevenue    -0.0084        0.00      -0.0973       0.0804
# NewsRevenue    0.0759        0.08       0.0106       0.1412
# 
# Likelihood ratio tests about transformation parameters
# LRT df       pval
# LR test, lambda = (0 0 0)    6.615636  3 0.08521198

## Log transformation with Rounded power all zeros

tAdPages <- log(magazines$AdPages)
tSubRevenue <- log(magazines$SubRevenue)
tNewsRevenue <- log(magazines$NewsRevenue)
m1 <- lm(AdRevenue ~ log(AdPages) + log(SubRevenue) + log(NewsRevenue), magazines)
summary(m1)

inverseResponsePlot(m1)

# lambda          RSS
# 1  0.2308236 245941272773
# 2 -1.0000000 892293703420
# 3  0.0000000 279043725925
# 4  1.0000000 521866609845



## Approach - 2

summary(a2 <- powerTransform(cbind(AdRevenue, AdPages, SubRevenue, NewsRevenue) ~
                               -1, magazines))

# bcPower Transformations to Multinormality 
# Est Power Rounded Pwr Wald Lwr bnd Wald Upr Bnd
# AdRevenue      0.1071        0.11       0.0299       0.1843
# AdPages        0.0883        0.00      -0.0755       0.2521
# SubRevenue    -0.0153        0.00      -0.0862       0.0557
# NewsRevenue    0.0763        0.08       0.0115       0.1410
# 
# Likelihood ratio tests about transformation parameters
# LRT df        pval
# LR test, lambda = (0 0 0 0)   13.87021  4 0.007721018
# LR test, lambda = (1 1 1 1) 1540.50928  4 0.000000000

## All log plots

pairs(log(magazines$AdRevenue) ~ tAdPages + tSubRevenue + tNewsRevenue)

m2 <- lm(log(AdRevenue) ~ log(AdPages) + log(SubRevenue) + log(NewsRevenue), magazines)
par(mfrow = c(2, 2))
StanRes2 <- rstandard(m2)
plot(tAdPages, StanRes2, ylab = "Standardized Residuals")
plot(tSubRevenue, StanRes2, ylab = "Standardized Residuals")
plot(tNewsRevenue, StanRes2, ylab = "Standardized Residuals")
plot(m2$fitted.values, StanRes2, ylab = "Standardized Residuals", xlab = "Fitted Values")

plot(m2$fitted.values, log(magazines$AdRevenue), xlab = "Fitted Values")
abline(lsfit(m2$fitted.values, log(magazines$AdRevenue)))


summary(m2)

# Call:
#   lm(formula = log(AdRevenue) ~ log(AdPages) + log(SubRevenue) + 
#        log(NewsRevenue), data = magazines)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.29980 -0.25827  0.04816  0.33238  0.85795 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -2.02894    0.41407  -4.900 1.98e-06 ***
#   log(AdPages)      1.02918    0.05564  18.497  < 2e-16 ***
#   log(SubRevenue)   0.55849    0.03159  17.677  < 2e-16 ***
#   log(NewsRevenue)  0.04109    0.02414   1.702   0.0903 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4483 on 200 degrees of freedom
# Multiple R-squared:  0.8326,	Adjusted R-squared:  0.8301 
# F-statistic: 331.6 on 3 and 200 DF,  p-value: < 2.2e-16