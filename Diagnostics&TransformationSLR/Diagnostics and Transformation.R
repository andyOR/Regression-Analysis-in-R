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