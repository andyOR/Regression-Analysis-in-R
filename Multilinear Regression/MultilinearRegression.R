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


