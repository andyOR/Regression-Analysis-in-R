## Reading table in R
production <- read.table("production.txt", header = TRUE)
production # look at dataset
head(production, 10) # look at the data for large data sets


# summary for the 2nd and 3rd columns of the data frame
summary(production[, 2:3]) 
# RunTime         RunSize     
# Min.   :147.0   Min.   : 58.0  
# 1st Qu.:171.2   1st Qu.:120.8  
# Median :207.5   Median :182.0  
# Mean   :202.1   Mean   :201.8  
# 3rd Qu.:226.2   3rd Qu.:278.8  
# Max.   :253.0   Max.   :344.0  


## scatter plot of related variables
plot(production$RunSize, production$RunTime, xlab = "Run Size", ylab = "Run Time", title("RunSize Vs. RunTime"))



## Estimation of the regression parameters ??0, ??1, and ??2
## Yi = ??0 + ??1Xi + ei
# Fit the regression model
m1 <- lm(RunTime ~ RunSize, data = production) 
summary(m1) # Get regression results

# Call:
#   lm(formula = RunTime ~ RunSize, data = production)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -28.597 -11.079   3.329   8.302  29.627 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 149.74770    8.32815   17.98 6.00e-13 ***
#   RunSize       0.25924    0.03714    6.98 1.61e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 16.25 on 18 degrees of freedom
# Multiple R-squared:  0.7302,	Adjusted R-squared:  0.7152 
# F-statistic: 48.72 on 1 and 18 DF,  p-value: 1.615e-06


# Create a table with fitted values and residuals
data.frame(production, fitted.value = fitted(m1), residual = resid(m1))

# Case RunTime RunSize fitted.value    residual
# 1     1     195     175     195.1152  -0.1152469
# 2     2     215     189     198.7447  16.2553496
# 3     3     243     344     238.9273   4.0726679
# 4     4     162      88     172.5611 -10.5610965
# 5     5     185     114     179.3014   5.6985827
# 6     6     231     338     237.3719  -6.3718734
# 7     7     234     271     220.0026  13.9974148
# 8     8     166     173     194.5968 -28.5967607
# 9     9     253     284     223.3727  29.6272544
# 10   10     196     277     221.5580 -25.5580439
# 11   11     220     337     237.1126 -17.1126303
# 12   12     168      58     164.7838   3.2161967
# 13   13     207     146     187.5972  19.4028033
# 14   14     225     277     221.5580   3.4419561
# 15   15     169     123     181.6346 -12.6346053
# 16   16     215     227     208.5959   6.4041115
# 17   17     147      63     166.0800 -19.0800188
# 18   18     230     337     237.1126  -7.1126303
# 19   19     208     146     187.5972  20.4028033
# 20   20     172      68     167.3762   4.6237657


## Hypothesis test for significance of estimates for ??0, and ??1


# For ??1
# H0: ??1 = 0 vs. H1: ??1 ??? 0
# Reject at the alpha level if |T| > tval. tval can be computed in R
alpha = 0.05
n= 20# no of sample
p = 2#no of estimators
# degrees of freedom = sample size - number of mean parameters estimated
tval = qt(1 - alpha/2, n - p)
tval
# [1] 2.100922
# T for ??1 = 17.98 and so T > tval

# confidence interval: a range of values that we are confident the true parameter falls into
## confidence interval for ??1

alpha = 0.05 #type I error
n = 20 #sample size
tval = qt(1 - alpha/2, n - 2) #quantile of t-distribution with d.f. n-2
lower = summary(m1)[4]$coefficients[2, 1] - tval * summary(m1)[4]$coefficients[2,
                                                                               2]
upper = summary(m1)[4]$coefficients[2, 1] + tval * summary(m1)[4]$coefficients[2,
                                                                               2]
c(lower, upper) # 95% confidence interval

# [1] 0.1812107 0.3372755



# For ??0
# H0: ??0 = 0 vs. H1: ??0 ??? 0
# T for ??0 is 6.97

#Confidence interval for ??0

alpha = 0.05 #type I error
n = 20 #sample size
tval = qt(1 - alpha/2, n - 2) #quantile of t-distribution with d.f. n-2
lower = summary(m1)[4]$coefficients[1, 1] - tval * summary(m1)[4]$coefficients[1,
                                                                               2]
upper = summary(m1)[4]$coefficients[1, 1] + tval * summary(m1)[4]$coefficients[1,
                                                                               2]
c(lower, upper)
# [1] 132.2509 167.2445

# 95% confidence intervals
confint(m1, level = 0.95)

#                 2.5 %      97.5 %
#   (Intercept) 132.2509062 167.2444999
# RunSize       0.1812107   0.3372755


# Prediction Intervals: uncertainty about future observations
# The prediction interval is wider than the corresponding confidence interval

# New run size = 50,100,150,200,250,300,350
(CI <- predict(m1, newdata = data.frame(RunSize = c(50, 100, 150, 200, 250, 300,
                                                    350)), interval = "confidence", level = 0.95))
# fit      lwr      upr
# 1 162.7099 148.6204 176.7994
# 2 175.6720 164.6568 186.6872
# 3 188.6342 179.9969 197.2714
# 4 201.5963 193.9600 209.2326
# 5 214.5585 206.0455 223.0714
# 6 227.5206 216.7006 238.3407
# 7 240.4828 226.6220 254.3435

(PI <- predict(m1, newdata = data.frame(RunSize = c(50, 100, 150, 200, 250, 300,
                                                    350)), interval = "prediction", level = 0.95))
# fit      lwr      upr
# 1 162.7099 125.7720 199.6478
# 2 175.6720 139.7940 211.5500
# 3 188.6342 153.4135 223.8548
# 4 201.5963 166.6076 236.5850
# 5 214.5585 179.3681 249.7489
# 6 227.5206 191.7021 263.3392
# 7 240.4828 203.6315 277.3340

## Plotting results of CI and PI
plot(production$RunSize, production$RunTime, ylim = range(production$RunSize, PI))
matlines(c(50, 100, 150, 200, 250, 300, 350), CI, lty = c(1, 2, 2), lwd = 1.5, col = "blue")
matlines(c(50, 100, 150, 200, 250, 300, 350), PI, lty = c(1, 3, 3), lwd = 1.5, col = "red")
legend(b = "CI")



## END