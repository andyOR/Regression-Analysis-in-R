## Histogram

set.seed(12345)
x <- rnorm(200, 100, 10)
hist(x) # the height of the bar gives you the frequency
hist(x, probability = TRUE) # the height of the bar is proportion (or probability)
par(mfrow = c(1, 2))
hist(x, breaks = 10) # 10 breaks

## Add Density plot
lines(density(x), lwd = 2, col = "blue") # Kernel Density Estimation

## Aesthetics 
hist(x, breaks = 10, freq = F, col = "gray", main = "Histogram and density estimate",
     xlab = "weight")


## Box Plot

y <- x[1:40]
boxplot(y)

hist(y, xlim = c(80, max(y)), col = "pink", main = "Histogram of weight", xlab = "weight")
boxplot(y, horizontal = TRUE, outline = TRUE, ylim = c(80, max(y)), frame = F, col = "cadetblue1",
        add = TRUE)


## Normal quantile plot

qqnorm(y)
qqnorm(y, main = "Normal Q-Q Plot of weight", xlab = "Theoretical Quantiles of weight",
       ylab = "Sample Quantiles of weight")
qqline(y, col = "red", lwd = 2) # Addding theoretical line
par(mfrow = c(1,1))


##