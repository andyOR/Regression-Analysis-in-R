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


## simple Linear Regression
