x <- c(1.1, 2.9, 4.1, 5.2, 5.3)
x_mean <- mean(x)
x_var <- var(x)

cat("X Mean: ", x_mean, "\n")
cat("X Variance: ", x_var, "\n")

set.seed(2020)
y <- rnorm(10)
y_mean <- mean(y)

cat("Y Mean: ", y_mean, "\n")
