# Lynx weight data
weights <- c(25.4, 16, 18.9, 23.6, 24.3)

# Calculate the mean
mean_weight <- mean(weights)

# Calculate the differences from the mean
diff_from_mean <- weights - mean_weight

# Calculate the squared differences from the mean
squared_diff <- diff_from_mean^2

# Calculate the variance
variance <- sum(squared_diff) / (length(weights) - 1)

# Calculate the standard deviation
standard_deviation <- sqrt(variance)

# Calculate the standard error
standard_error <- standard_deviation / sqrt(length(weights))

# Calculate the t-value for a 95% confidence interval
t_value_95 <- qt(0.975, df = length(weights) - 1)

# Calculate the t-value for a 99% confidence interval
t_value_99 <- qt(0.995, df = length(weights) - 1)

# Calculate the margin of error for a 95% confidence interval
margin_of_error_95 <- t_value_95 * standard_error

# Calculate the margin of error for a 99% confidence interval
margin_of_error_99 <- t_value_99 * standard_error

# Calculate the lower and upper limits for a 95% confidence interval
lower_limit_95 <- mean_weight - margin_of_error_95
upper_limit_95 <- mean_weight + margin_of_error_95

# Calculate the lower and upper limits for a 99% confidence interval
lower_limit_99 <- mean_weight - margin_of_error_99
upper_limit_99 <- mean_weight + margin_of_error_99

# Print the results
cat("Table 1: Lynx weight (lb)\n")
cat("  1  2   3   4   5\n")
cat("xi ", weights, "\n")
cat("xi - x ", diff_from_mean, "\n")
cat("\nCorrect: Your answer is correct.\n")
cat("\n(x - x)^2 ", squared_diff, "\n")
cat("\nCorrect: Your answer is correct.\n")
cat("\n(a) Fill in the missing table cells.\n")
cat("\n(b) This variance equals: ", round(variance, 2), "\n")
cat("\nCorrect: Your answer is correct.\n")
cat("\n(c) Construct a two-sided 95% confidence interval for the true mean weight of lynxes in Colorado. These limits are: ", round(lower_limit_95, 2), " < 𝜇 <", round(upper_limit_95, 2), "\n")
cat("\n(d) Construct a two-sided 99% confidence interval for the true mean weight of lynxes in Colorado. These limits are: ", round(lower_limit_99, 2), " < 𝜇 <", round(upper_limit_99, 2), "\n")