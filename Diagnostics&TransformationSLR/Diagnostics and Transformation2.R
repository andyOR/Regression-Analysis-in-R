# Standardized residuals are used for detecting outliers (in y direction)
# Outlier is a point whose response variable is far from where the general regression relationship would imply.


# the relationship between bid price (response) and coupon payment (predictor)
bonds <- read.table("bonds.txt", header = TRUE)
attach(bonds)
head(bonds)

# Considering simple linear regression
m1 <- lm(BidPrice ~ CouponRate)
summary(m1)

# Call:
#   lm(formula = BidPrice ~ CouponRate)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -8.249 -2.470 -0.838  2.550 10.515 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  74.7866     2.8267  26.458  < 2e-16 ***
#   CouponRate    3.0661     0.3068   9.994 1.64e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.175 on 33 degrees of freedom
# Multiple R-squared:  0.7516,	Adjusted R-squared:  0.7441 
# F-statistic: 99.87 on 1 and 33 DF,  p-value: 1.645e-11

plot(CouponRate, BidPrice, xlab = "Coupon Rate (%)", ylab = "Bid Price ($)", ylim = c(85,
                                                                                      120), xlim = c(2, 14))
abline(lsfit(CouponRate, BidPrice), col = 'red')

# Outliers are leverage points whose standardized residuals fall outside the interval [-2,2]
# Bad leverage point is a leverage point whose standardized residual falls outside
# the interval [-2,2].
# Good leverage point is a leverage point whose standardized residual falls inside
# the interval [-2,2]

leverage1 <- hatvalues(m1) # hat values for leverage
StanRes1 <- rstandard(m1) #standard residuals
residual1 <- m1$residuals # residuals
par(mfrow = c(1, 2))
plot(CouponRate, residual1, main = 'Residuals')
plot(CouponRate, StanRes1, main =  'Standard Residuals')
abline(h=2, col = 'red')
Case[which(abs(StanRes1) > 2)] # Identify outliers
# [1] 13 34 35

Case[which(leverage1 > 4/nrow(bonds))] # leverage points
# [1]  4  5 13 35

# Cases 13 and 35 are points of high leverage that are also outliers. So they are badleverage points


## SLR after considering leverage points ('4, 13, and 35 cases are flower bonds with tax advantages)

m2 <- update(m1, subset = (1:35)[-c(4, 13, 35)])
summary(m2)

# Call:
#   lm(formula = BidPrice ~ CouponRate, subset = (1:35)[-c(4, 13, 
#                                                          35)])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1301 -0.3789  0.2240  0.4576  1.8099 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  57.2932     1.0358   55.31   <2e-16 ***
#   CouponRate    4.8338     0.1082   44.67   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.024 on 30 degrees of freedom
# Multiple R-squared:  0.9852,	Adjusted R-squared:  0.9847 
# F-statistic:  1996 on 1 and 30 DF,  p-value: < 2.2e-16

# PLot without high leverage points (4, 13, and 35)
par(mfrow = c(1,1))
plot(CouponRate[-c(4, 13, 35)], BidPrice[-c(4, 13, 35)], xlab = "Coupon Rate (%)",
     ylab = "Bid Price ($)", ylim = c(85, 120), xlim = c(2, 14), main = "Regular Bonds")
abline(m2, col= 'red')
StanRes2 <- rstandard(m2)
plot(CouponRate[-c(4, 13, 35)], StanRes2, xlab = "Coupon Rate (%)", ylab = "Standardized Residuals", xlim = c(2, 14), main = "Regular Bonds")
abline(h = 2, lty = 2, col = 'red')
abline(h = -2, lty = 2, col = 'red')

# Handling outliers and leverage points
# Points should not be routinely deleted from an analysis just because they do not fit
# the model. Outliers and bad leverage points are signals, flagging potential
# problems with the model. 
# Outliers often point out an important feature of the problem not considered
# before. They may point to an alternative model in which the points are not an
# outlier. In this case it is then worth considering fitting an alternative model


# Assessing the influence of certain cases
# one or more points can strongly control or influence the least squares fit of a
# regression model.
# Influence can be thought of as the product of leverage and outlierness

# Cook's Distance : a summary statistic that measures the influence of a single case on theleast squares fit of a regression line
# Large Di's indicate influential observations. Rough cut-off for SLR is 4/(n-2)
cd1 <- cooks.distance(m1)
plot(CouponRate, cd1, ylab = "Cook's Distance", main = 'Inluential Points on Cook\'s D')
text(CouponRate, cd1, labels = Case, cex = 0.6, pos = 3)
abline(h = 4/(35 - 2), lty = 2, col = 'red')

# Note: Di's only detects influence of single observation; Influential pairs may go
# undetected.
# Cook's D can be "fooled" by multiple outliers.
