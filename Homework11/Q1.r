# Define the x and y vectors
x <- c(1, 4, 9, 16, 25)
y <- c(0.1, 4.81, 9.19, 15.59, 25.78)

# Calculate the mean of x and y
x_mean <- mean(x)
y_mean <- mean(y)

# Calculate the differences from the mean
x_diff <- x - x_mean
y_diff <- y - y_mean

# Calculate the squared differences from the mean
x_diff_squared <- (x - x_mean)^2
y_diff_squared <- (y - y_mean)^2

# Calculate the product of the differences
xy_diff <- x_diff * y_diff

# Calculate the slope of the regression line
b1 <- sum(xy_diff) / sum(x_diff_squared)

# Create a data frame to store the intermediate calculations
data <- data.frame(x, y, x_mean, y_mean, x_diff, y_diff, x_diff_squared, y_diff_squared, xy_diff)
print(data)

# Calculate the intercept of the regression line
b0 = y_mean - b1 * x_mean

# Create a string representation of the regression equation
output <- paste("Regression equation: y =", round(b0, 5), "+", round(b1, 5), "* x")
print(output)

# Check if b0 - b1 < b0 + b1
print(paste(b0 - b1, " < ", b0 + b1))

# Define new x and y vectors
x_new <- c(1, 4, 9, 16, 25)
y_new <- c(0.1, 4.81, 9.19, 15.59, 25.78)

# Calculate the mean of the new x and y vectors
x_new_mean <- mean(x_new)
y_new_mean <- mean(y_new)

# Calculate the differences from the new means
x_new_diff <- x_new - x_new_mean
y_new_diff <- y_new - y_new_mean

# Calculate the squared differences from the new means
x_new_diff_squared <- (x_new - x_new_mean)^2
y_new_diff_squared <- (y_new - y_new_mean)^2

# Create a data frame to store the intermediate calculations for the new data
data_new <- data.frame(x_new, y_new, x_new_mean, y_new_mean, x_new_diff, y_new_diff, x_new_diff_squared, y_new_diff_squared)
print(data_new)

# Calculate the slope of the regression line for the new data
b1_new = sum(x_new_diff * y_new_diff) / sum(x_new_diff_squared)

# Calculate the intercept of the regression line for the new data
b0_new = y_new_mean - b1_new * x_new_mean

# Create a string representation of the regression equation for the new data
output_new <- paste("Regression equation: y =", round(b0_new, 5), "+", round(b1_new, 5), "* x")
print(output_new)

# Calculate the standard error of the slope estimate
se_b1 <- sqrt(SSE / ((length(x_new) - 2) * sum(x_new_diff_squared)))

# Calculate the margin of error
margin_of_error <- qt(0.975, df = length(x_new) - 2) * se_b1

# Calculate the lower and upper bounds of the confidence interval
lower_bound <- b1_new - margin_of_error
upper_bound <- b1_new + margin_of_error

# Create the confidence interval
confidence_interval <- c(lower_bound, upper_bound)

# Print the confidence interval
print(paste("95% Confidence Interval for b1:", confidence_interval))

# Calculate SSR (Sum of Squares Regression)
SSR <- sum((b0_new + b1_new * x_new - y_new_mean)^2)

# Calculate SSE (Sum of Squares Error)
SSE <- sum((y_new - (b0_new + b1_new * x_new))^2)

# Calculate R-squared
R_squared <- 1 - SSE / sum((y_new - y_new_mean)^2)

# Print SSR, SSE, and R-squared
print(paste("SSR:", SSR))
print(paste("SSE:", SSE))
print(paste("R-squared:", R_squared))

# Step 1: State the null and alternative hypotheses
null_hypothesis <- "The slope of the regression line is zero (b1 = 0)"
alternative_hypothesis <- "The slope of the regression line is not zero (b1 != 0)"

# Step 2: Set the significance level (a)
a <- 0.01

# Step 3: Calculate the test statistic
t_stat <- b1_new / (sqrt(SSE / (length(x_new) - 2)) * sqrt(sum(x_new_diff_squared) / ((length(x_new) - 1) * sum(x_new_diff_squared))))
print(paste("t-statistic:", t_stat))

# Step 4: Determine the critical value(s)
critical_value <- qt(1 - a/2, df = length(x_new) - 2)
p_value <- 2 * pt(-abs(t_stat), df = length(x_new) - 2)
print(paste("p-value:", p_value))

# Step 5: Make a decision
if (p_value > a) {
    decision <- "Reject the null hypothesis"
} else {
    decision <- "Fail to reject the null hypothesis"
}

# Step 6: State the conclusion
conclusion <- paste("At the", a * 100, "% significance level, we", decision, "and conclude that", alternative_hypothesis)

print(conclusion)