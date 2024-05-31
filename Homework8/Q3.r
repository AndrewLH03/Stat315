# Define sample size, mean of the sample, and true mean
sampleSize <- 103
meanSample <- 60 / sampleSize
trueMean <- 0.5

# Calculate the z-value using the formula
z_value <- (meanSample - trueMean) / sqrt(trueMean * (1 - trueMean) / sampleSize)

# Calculate the p-value using the cumulative distribution function
p_value <- pnorm(z_value)

# Output the z-value
cat("The z-value is:", z_value, "\n")

# Output the p-value
cat("The p-value is:", p_value, "\n")

# Conduct hypothesis test at alpha = 0.05
alpha <- 0.05
if (p_value < alpha) {
    cat("Reject the null hypothesis")
} else {
    cat("Fail to reject the null hypothesis")
}

