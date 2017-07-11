# Transformation and its uses

# Overcome problems due to nonconstant variance
# Estimate percentage effects
# Overcome problems due to nonlinearity
cleaning <- read.table("cleaning.txt", header = TRUE)
attach(cleaning)

#square root transformation of both X and Y
sqrtcrews <- sqrt(Crews)
sqrtrooms <- sqrt(Rooms)
m2 <- lm(sqrtrooms ~ sqrtcrews)
summary(m2)


# Call:
#   lm(formula = sqrtrooms ~ sqrtcrews)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.09825 -0.43988  0.06826  0.42726  1.20275 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.2001     0.2757   0.726    0.471    
# sqrtcrews     1.9016     0.0936  20.316   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.594 on 51 degrees of freedom
# Multiple R-squared:   0.89,	Adjusted R-squared:  0.8879 
# F-statistic: 412.7 on 1 and 51 DF,  p-value: < 2.2e-16

par(mfrow = c(1, 2))
plot(sqrt(Crews), sqrt(Rooms), xlab = "Square Root(Number of Crews)", ylab = "Square Root(Number of Rooms)", cex.lab = 1.1, main = "Square Root Trans.")
abline(lsfit(sqrt(Crews), sqrt(Rooms)), col = "blue")
StanRes2 <- rstandard(m2)
plot(sqrtcrews, StanRes2, xlab = "Square Root(Number of Crews)", ylab = "Standardized Residuals", cex.lab = 1.1, main = "Std. Residuals plot")


# Transformation for Percentage Effects
# The slope is approximately equals the ratio of the percentage changes in Y and x.
# For every 1% increase in x, the model predicts a Beta % increases in Y

# the effect of price on sales and to estimate the percentage effect on sales of a 1 % increase in price

confood1 <- read.table("confood1.txt", header = TRUE)
attach(confood1)
head(confood1)

# Week Sales Price
# 1    1   611  0.67
# 2    2   673  0.66
# 3    3   710  0.67
# 4    4   478  0.66
# 5    5   425  0.66
# 6    6   601  0.65

plot(Price, Sales, cex.lab =1.1, main = "Scatter plot of Price Vs. Sales", col.main = "blue")
abline(lsfit(Price, Sales), col = "blue")

# Logarithmic scale for quantify percentage effects
plot(log(Price), log(Sales), xlab = "log(Price)", ylab = "log(Sales)", main = "Scatter plot of Log(Price) Vs. Log(Sales)", col.main = "blue")
abline(lsfit(log(Price), log(Sales)), col = "blue")

## Regression model
m1 <- lm(log(Sales) ~ log(Price))
summary(m1)

# Call:
#   lm(formula = log(Sales) ~ log(Price))

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.88973 -0.18188  0.04025  0.22087  1.31026 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.8029     0.1744   27.53  < 2e-16 ***
#   log(Price)   -5.1477     0.5098  -10.10 1.16e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4013 on 50 degrees of freedom
# Multiple R-squared:  0.671,	Adjusted R-squared:  0.6644 
# F-statistic:   102 on 1 and 50 DF,  p-value: 1.159e-13

# Beta1 = -5.1: for every 1% increase in price there will be approximately a 5.1 % reduction in demand 
# Since the magnitude of Beta1 is greater than 1, the quantity demanded is said to be "elastic", meaning that a price change will cause an ever larger change in quantity demanded

StanRes1 <- rstandard(m1)
plot(log(Price), StanRes1, xlab = "log(Price)", ylab = "Standardized Residuals", col.main = "blue", cex.lab = 1.1, main = "Standard residuals for Log Price")


# two general methods for transforming the response variable Y and/or
# the predictor X to overcome problems due to nonlinearity:
# Inverse response plots
# Box-Cox procedure