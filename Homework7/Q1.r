# Define the sample statistics
sample_mean <- 106.2
sample_sd <- 40.3
sample_size <- 10

# Calculate the t-value
t_statistic <- (sample_mean - population_mean) / (sample_sd / sqrt(sample_size))

# Define the population mean (under null hypothesis)
population_mean <- 100

# Calculate the p-value
p_value <- pt(t_statistic, 9, lower.tail = FALSE)

# Print the t-statistic
cat("The t-statistic is:", t_statistic, "\n")
# Print the t-statistic
cat("The p-value is:", p_value, "\n")

# Conduct hypothesis test at alpha = 0.05
alpha <- 0.05
if (p_value < alpha) {
    cat("Reject the null hypothesis")
} else {
    cat("Fail to reject the null hypothesis")
}