# Sample sizes and counts
n_fc <- 174
n_boulder <- 148
count_fc <- 155
count_boulder <- 39

# Calculate sample proportions
prop_fc <- round(count_fc / n_fc, 2)
prop_boulder <- round(count_boulder / n_boulder, 2)

# Calculate standard errors
se_fc <- sqrt(prop_fc * (1 - prop_fc) / n_fc)
se_boulder <- sqrt(prop_boulder * (1 - prop_boulder) / n_boulder)

# Calculate the difference in proportions
diff_prop <- prop_fc - prop_boulder

# Calculate the margin of error
z <- qnorm(0.975)  # 95% confidence level
me <- z * sqrt(se_fc^2 + se_boulder^2)

# Calculate the confidence interval
lower <- round(diff_prop - me, 2)
upper <- round(diff_prop + me, 2)

# Print the results
cat("Sample proportion of Fort Collins residents supporting their local government:", prop_fc, "\n")
cat("Sample proportion of Boulder residents supporting their local government:", prop_boulder, "\n")
cat("95% confidence interval for the difference in proportions:", lower, ",", upper, "\n")