# Given values
x_new <- 1
MSE <- 0.1645
Sxx <- 3170091
n <- 24

# Calculate the standard error
se <- sqrt(MSE * (1/n + (1/Sxx) * (x_new - mean(data$Velocity))^2))

# Calculate the t-value for a
# Calculate y_mean, y_diff, y_diff_squared for y
y_mean <- mean(data$Distance)
y_diff <- data$Distance - y_mean
y_diff_squared <- y_diff^2

# Calculate b1 and b0
b1 <- sum(x_diff * y_diff) / sum(x_diff_squared)
b0 <- y_mean - b1 * x_mean

# Print the model summary
summary(model)

# Output the modeled equation
cat("Modeled equation: y =", round(b0, 4), "+", round(b1, 4), "* x\n")

# Step 1: State the null and alternative hypotheses
null_hypothesis <- "The slope of the regression line is zero (b1 = 0)"
alternative_hypothesis <- "The slope of the regression line is not zero (b1 ≠ 0)"

# Step 2: Set the significance level (a)
a <- 0.05

# Step 3: Calculate the test statistic
# Calculate b1_new
b1_new <- sum(x_new_diff * y_new_diff) / sum(x_new_diff_squared)

# Calculate SSE
SSE <- sum(y_new_diff_squared)

# Calculate t_stat
t_stat <- b1_new / (sqrt(SSE / (length(x_new) - 2)) * sqrt(sum(x_new_diff_squared) / ((length(x_new) - 1) * sum(x_new_diff_squared))))

print(paste("t-statistic:", t_stat))

# Step 4: Calculate the p-value
p_value <- 2 * pt(abs(test_statistic), df = length(data$Velocity) - 2, lower.tail = FALSE)

# Step 5: Make a decision
if (p_value < a) {
    decision <- "Reject the null hypothesis"
} else {
    decision <- "Fail to reject the null hypothesis"
}

# Step 6: State the conclusion
conclusion <- paste("Based on the hypothesis test with a significance level of", a, ", we", decision, "and conclude that", ifelse(decision == "Reject the null hypothesis", "there is sufficient evidence to suggest that the slope of the regression line is not zero.", "there is not sufficient evidence to suggest that the slope of the regression line is not zero."))

# Print the hypothesis test results
cat("Null Hypothesis:", null_hypothesis, "\n")
cat("Alternative Hypothesis:", alternative_hypothesis, "\n")
cat("Significance Level (a):", a, "\n")
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")
cat("Decision:", decision, "\n")
cat("Conclusion:", conclusion, "\n")

# Given values
x <- 373.125
MSE <- 0.1645
Sxx <- 3170091
n <- 24
# Calculate x_diff and x_diff_squared
x_diff <- data$Velocity - mean(data$Velocity)
x_diff_squared <- x_diff^2


# Calculate the standard error
se <- sqrt(MSE * (1/n + (1 - x_mean)^2 / (sum(x_diff_squared))))

# Calculate the t-value for a 95% confidence interval
t_value <- qt(0.975, df = n - 2)

# Calculate the margin of error
margin_of_error <- t_value * se

# Calculate the lower and upper bounds of the confidence interval
lower_bound <- mean(data$Distance) - margin_of_error
upper_bound <- mean(data$Distance) + margin_of_error

# Print the confidence interval
cat("Lower:", round(lower_bound, 3), "Mpc\n")
cat("Upper:", round(upper_bound, 3), "Mpc\n")

SE = sqrt(MSE * (1 + 1/n + (1 - x_mean)^2 / sum(x_diff_squared)))

# Calculate the prediction interval
lower_bound_pred <- b0 + b1 * x_new - qt(0.975, df = n - 2) * se
upper_bound_pred <- b0 + b1 * x_new + qt(0.975, df = n - 2) * se

# Print the prediction interval
cat("Lower bound of the prediction interval:", round(lower_bound_pred, 3), "\n")
cat("Upper bound of the prediction interval:", round(upper_bound_pred, 3), "\n")
