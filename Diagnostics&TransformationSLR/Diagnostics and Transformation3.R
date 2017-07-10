## Normality of errors
#The assumption is generally checked by looking at the normal probability plot (normal Q-Q plot) of the standardized residuals

## Constant Variance
# A crucial assumption in any regression analysis is that the errors have constant variance
# We can study this by looking at the plot of (standardized) residuals vs. X or plot of (standardized) residuals vs. fitted values

# Regression equation to model the relationship between the number of rooms cleaned and the number of crews
cleaning <- read.table("cleaning.txt", header = TRUE)
attach(cleaning)
head(cleaning, 10)

#Basic scatter plot
plot(Crews, Rooms, xlab = "Number of Crews", ylab = "Number of Rooms Cleaned")
abline(lsfit(Crews, Rooms), col = "blue")
m1 <- lm(Rooms ~ Crews)
summary(m1)

# Call:
#   lm(formula = Rooms ~ Crews)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -15.9990  -4.9901   0.8046   4.0010  17.0010 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.7847     2.0965   0.851    0.399    
# Crews         3.7009     0.2118  17.472   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 7.336 on 51 degrees of freedom
# Multiple R-squared:  0.8569,	Adjusted R-squared:  0.854 
# F-statistic: 305.3 on 1 and 51 DF,  p-value: < 2.2e-16

# predict the number of rooms that can be cleaned by crews 4 and by 16 crews
predict(m1, newdata = data.frame(Crews = c(4, 16)), interval = "prediction", level = 0.95)

# fit      lwr      upr
# 1 16.58827  1.58941 31.58713
# 2 60.99899 45.81025 76.18773

# Standarized Residuals
StanRes1 <- rstandard(m1)
plot(Crews, StanRes1, xlab = "Number of Crews", ylab = "Standardized Residuals", main="Standard Residuals", col.main="blue", cex.lab = 1.2)


## Diagnostics Plots of Regression
par(mfrow = c(2, 2))
plot(m1)


## Regression Model Assumptions
# 1. Linearity: The relationship between X and the mean of Y is linear
# 2. Constant variance: The variance of residual is the same for any value of X
# 3. Independence: Observations are independent of each other. (Typically a problem
#                                                               in time series data)
# 4. Normality: For any fixed value x, Y is normally distributed.