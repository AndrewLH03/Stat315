
# Sample size
n <- 1000

# Number of people who hung up in the first minute
x <- 577

# Confidence level
confidence <- 0.95

# Calculate the sample proportion
p_hat <- x / n

# Calculate the standard error
se <- sqrt(p_hat * (1 - p_hat) / n)

# Calculate the margin of error
me <- qnorm(1 - (1 - confidence) / 2) * se

# Calculate the lower and upper bounds of the confidence interval
lower_bound <- p_hat - me
upper_bound <- p_hat + me

# Round the results to three decimal places
lower_bound <- round(lower_bound, 3)
upper_bound <- round(upper_bound, 3)

# Print the confidence interval
cat("The two-sided", confidence * 100, "% confidence interval for p is", lower_bound, "< p <", upper_bound, ".\n")

# Calculate the confidence interval for a different confidence level
confidence <- 0.86

# Calculate the margin of error
me <- qnorm(1 - (1 - confidence) / 2) * se

# Calculate the lower and upper bounds of the confidence interval
lower_bound <- p_hat - me
upper_bound <- p_hat + me

# Round the results to three decimal places
lower_bound <- round(lower_bound, 3)
upper_bound <- round(upper_bound, 3)

# Print the confidence interval
cat("The two-sided", confidence * 100, "% confidence interval for p is", lower_bound, "< p <", upper_bound, ".\n")

# Calculate the lower bound for an upper 95% confidence interval
confidence <- 0.95

# Calculate the margin of error
me <- qnorm(1 - confidence) * se

# Calculate the lower bound
lower_bound <- p_hat - me

# Round the result to three decimal places
lower_bound <- round(lower_bound, 3)

# Print the lower bound
cat("The lower bound for an upper", confidence * 100, "% confidence interval for p is", lower_bound, ".\n")

# Calculate the upper bound for a lower 99% confidence interval
confidence <- 0.99

# Calculate the margin of error
me <- qnorm(1 - confidence) * se

# Calculate the upper bound
upper_bound <- p_hat + me

# Round the result to three decimal places
upper_bound <- round(upper_bound, 3)

# Print the upper bound
cat("The upper bound for a lower", confidence * 100, "% confidence interval for p is", upper_bound, ".\n")