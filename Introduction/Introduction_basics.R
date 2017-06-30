a <- 2
a

# We can do calculation with a
a * 5 

# Assign a new value to a
a <- a + 10

#To remove all variables from R's memory
rm(list = ls())

#concatenate (paste together) elements
foodexp <- c(145, 214, 100, 300) 
income <- c(5000, 7000, 4901, 8503)
exp_food <- foodexp/income
exp_food

# Adding strings
factor <- c("low", "mid", "high")
factor

# sequential list
veca = seq(from = 0, to = 1, by = 0.25)
veca
vecb = seq(-1, 1, 0.5)

# how many elements of vec2
length(vecb)
veca + vecb
sum(veca)
mean(veca)
quantile(veca)

## matrix form
mat <- matrix(data = c(1, 2, 3, 4, 5, 6), ncol = 3)
mat

# first row and all columns
mat[1, ]
mat[2, 3]
sum(mat)

# col sum
colSums(mat)

# row sum
rowSums(mat)

# number of columns
ncol(mat)

## conditional statements
w <- 10
if (w > 100) {
  s <- 2
} else {
  s <- 10
}
s

## For Loops
n <- 100
x <- runif(n)
y <- x^2
y[1:10]

y <- rep(0, n)
for (i in 1:n) {
  y[i] <- (x[i])^2
} 
y[1:10]

# While loop
counter <- 1
while (counter <= n) {
  y[counter] <- (x[counter])^2
  counter <- counter + 1
} 
y[1:10]
