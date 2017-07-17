## Multiple linear regression is an extension of simple linear regression.

## Product demand models
# demand depends on competitors' prices, advertising, demographics

## Multi-factor asset pricing models
# Fama-French three-factor model
# Uses three variables to describe the returns of a portfolio or stock


## Polynomial Regression
# Modeling salary from years of experience

profsalary <- read.table("profsalary.txt", header = TRUE)
attach(profsalary)
str(profsalary)

# 'data.frame':	143 obs. of  3 variables:
#   $ Case      : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Salary    : int  71 69 73 69 65 75 66 66 67 69 ...
# $ Experience: int  26 19 22 17 13 25 35 16 16 16 ...

plot(Experience, Salary, xlab = "Years of Experience", main = "Ploynomial Regression", cex.lab = 1.2)
m1 <- lm(Salary ~ Experience) # SLR
leverage1 <- hatvalues(m1)
StanRes1 <- rstandard(m1)
ExperienceNew <- seq(0, 37, len = 37)
plot(Experience, StanRes1, xlab = "Years of Experience", ylab = "Standardized Residuals", main = "Residuals Pattern", cex.lab = 1.2, col.lab = "blue")


m2 <- lm(Salary ~ Experience + I(Experience^2))# Polynomial Regression
plot(Experience, Salary, xlab = "Years of Experience", cex.lab = 1.2, main = "Plot with Polynomial terms", col.lab = "blue")
ExperienceNew <- seq(0, 37, len = 37)
lines(ExperienceNew, predict(m2, newdata = data.frame(Experience = ExperienceNew)), col = "red")

StanRes2 <- rstandard(m2)
plot(Experience, StanRes2, xlab = "Years of Experience", ylab = "Standardized Residuals", main = "Residuals after Polynomial terms", cex.lab = 1.2, col.lab = "blue")
leverage2 <- hatvalues(m2)
plot(Experience, leverage2, xlab = "Years of Experience", ylab = "Leverage",  cex.lab = 1.2, main = "Leverage Points", col.main = "blue")
abline(h = 6/max(Case), lty = 2)

par(mfrow = c(2, 2))
plot(m2)



## Estimation in MLR

# Types of hypothesis tests
# Overall test
# Test for addition of a single variable
# Test for addition of a group of variables


qf(0.95, df1 = 4, df2 = 5)
pf(167.37, df1 = 1, df2 = 17, lower.tail = F)
# R Dataset, stackloss - Operational data of a plant for the oxidation of ammonia to nitric acid.
str(stackloss)

# 'data.frame':	21 obs. of  4 variables:
#   $ Air.Flow  : num  80 80 75 62 62 62 62 62 58 58 ...
# $ Water.Temp: num  27 27 25 24 22 23 24 24 23 18 ...
# $ Acid.Conc.: num  89 88 90 87 87 87 93 93 87 80 ...
# $ stack.loss: num  42 37 37 28 18 18 19 20 15 14 ...

anova(lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss))

# Analysis of Variance Table
# 
# Response: stack.loss
# Df  Sum Sq Mean Sq  F value    Pr(>F)    
# Air.Flow    1 1750.12 1750.12 166.3707 3.309e-10 ***
#   Water.Temp  1  130.32  130.32  12.3886  0.002629 ** 
#   Acid.Conc.  1    9.97    9.97   0.9473  0.344046    
# Residuals  17  178.83   10.52                       
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


## R-squared Vs. Adj. R-sqaured

# Adding irrelevant predictor to the regression equation often increases R-square.
# The largest value of R-square will always occur when all the predictor variables are
# included, even if some don't significantly contribute to the model
# This can be completely misleading


## Generally it is better to look at adjusted R-sqaure rather than R-sqaure. 
# Adj. R-sqaured will penalize model that adds independent variables that do not fit the model


## Tests on invididual regression coefficients
# The F-test is always used first to test for the existence of a linear association Y between and any of the pX variables

summary(lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss))

# Call:
#   lm(formula = stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., 
#      data = stackloss)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.2377 -1.7117 -0.4551  2.3614  5.6978 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -39.9197    11.8960  -3.356  0.00375 ** 
#   Air.Flow      0.7156     0.1349   5.307  5.8e-05 ***
#   Water.Temp    1.2953     0.3680   3.520  0.00263 ** 
#   Acid.Conc.   -0.1521     0.1563  -0.973  0.34405    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.243 on 17 degrees of freedom
# Multiple R-squared:  0.9136,	Adjusted R-squared:  0.8983 
# F-statistic:  59.9 on 3 and 17 DF,  p-value: 3.016e-09


## Partial F-test for addition of variables

f.lm <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss)
r.lm <- lm(stack.loss ~ Air.Flow + Water.Temp, data = stackloss)
anova(r.lm, f.lm)

# Analysis of Variance Table
# 
# Model 1: stack.loss ~ Air.Flow + Water.Temp
# Model 2: stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     18 188.79                           
# 2     17 178.83  1    9.9654 0.9473  0.344

# Addition of variable Acid.Conc. does not add significance to the model

r1.lm <- lm(stack.loss ~ Air.Flow, data = stackloss)
f1.lm <- lm(stack.loss ~ Air.Flow + Water.Temp, data = stackloss)
anova(r1.lm, f1.lm) # Extra SS

# Analysis of Variance Table
# 
# Model 1: stack.loss ~ Air.Flow
# Model 2: stack.loss ~ Air.Flow + Water.Temp
# Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
# 1     19 319.12                                
# 2     18 188.80  1    130.32 12.425 0.002419 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

r2.lm <- lm(stack.loss ~ Water.Temp, data = stackloss)
anova(r2.lm, f1.lm)

# Analysis of Variance Table
# 
# Model 1: stack.loss ~ Water.Temp
# Model 2: stack.loss ~ Air.Flow + Water.Temp
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     19 483.15                                  
# 2     18 188.80  1    294.36 28.064 4.898e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


## Analysis of Covariance - multiple regression analysis in which there is at least one quantitative and one categorical explanatory variable

travel <- read.table("travel.txt", header = TRUE)
str(travel)

# 'data.frame':	925 obs. of  4 variables:
#   $ Amount : int  997 997 951 649 1265 1059 837 924 852 963 ...
# $ Age    : int  44 43 41 59 25 38 46 42 48 39 ...
# $ Segment: Factor w/ 2 levels "A","C": 1 1 1 1 1 1 1 1 1 1 ...
# $ C      : int  0 0 0 0 0 0 0 0 0 0 ...

# SLR
slr <- lm(Amount ~ Age, data = travel)
summary(slr)

# Call:
#   lm(formula = Amount ~ Age, data = travel)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -545.06 -199.03    6.34  198.74  497.39 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 957.9103    31.3056  30.599   <2e-16 ***
#   Age          -1.1140     0.6784  -1.642    0.101    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 237.7 on 923 degrees of freedom
# Multiple R-squared:  0.002913,	Adjusted R-squared:  0.001833 
# F-statistic: 2.697 on 1 and 923 DF,  p-value: 0.1009


plot(travel$Age, travel$Amount, xlab = "Age", ylab = "Amount", col = ifelse(travel$Segment ==
                                                                              "A", "red", "blue"), main = "Age Vs. Amount Plot with Segments", cex.lab = 1.2)
legend(24, 1000, pch = c(1, 1), col = c("red", "blue"), c("A", "B"), bty = "o", box.col = "white")


mlr1 <- lm(Amount ~ Age + Segment, data = travel)
summary(mlr1)

# Call:
#   lm(formula = Amount ~ Age + Segment, data = travel)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -538.1 -196.2    4.8  198.9  503.5 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 963.4254    32.0143  30.094   <2e-16 ***
#   Age          -1.0939     0.6789  -1.611    0.107    
# SegmentC    -12.9291    15.6455  -0.826    0.409    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 237.8 on 922 degrees of freedom
# Multiple R-squared:  0.003651,	Adjusted R-squared:  0.00149 
# F-statistic: 1.689 on 2 and 922 DF,  p-value: 0.1852


## Significant interaction btw variables

mlr2 <- lm(Amount ~ Age * Segment, data = travel)
summary(mlr2)

# Call:
#   lm(formula = Amount ~ Age * Segment, data = travel)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -143.298  -30.541   -0.034   31.108  130.743 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1814.5445     8.6011   211.0   <2e-16 ***
#   Age            -20.3175     0.1878  -108.2   <2e-16 ***
#   SegmentC     -1821.2337    12.5736  -144.8   <2e-16 ***
#   Age:SegmentC    40.4461     0.2724   148.5   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 47.63 on 921 degrees of freedom
# Multiple R-squared:  0.9601,	Adjusted R-squared:  0.9599 
# F-statistic:  7379 on 3 and 921 DF,  p-value: < 2.2e-16

anova(slr, mlr2)

# Analysis of Variance Table
# 
# Model 1: Amount ~ Age
# Model 2: Amount ~ Age * Segment
# Res.Df      RSS Df Sum of Sq     F    Pr(>F)    
# 1    923 52158945                                 
# 2    921  2089377  2  50069568 11035 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## END