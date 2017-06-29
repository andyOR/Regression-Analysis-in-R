a <- 2
a
a * 5 # We can do calculation with a
a <- a + 10 # Assign a new value to a
rm(list = ls()) #To remove all variables from R's memory
foodexp <- c(145, 214, 100, 300) #concatenate (paste together) elements
income <- c(5000, 7000, 4901, 8503)
exp_food <- foodexp/income
exp_food
factor <- c("low", "mid", "high")
factor
veca = seq(from = 0, to = 1, by = 0.25)
veca
vecb = seq(-1, 1, 0.5)
length(vecb) # how many elements of vec2
veca + vecb
sum(veca)
mean(veca)
quantile(veca)
mat <- matrix(data = c(1, 2, 3, 4, 5, 6), ncol = 3)# matrix form
mat
mat[1, ]# firs row and all columns
mat[2, 3]
sum(mat)
colSums(mat)# col sum
rowSums(mat)# row sum
ncol(mat)# number of columns
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
