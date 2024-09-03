# Part (a)
sample_size <- 11
sample_mean_reg <- 1841.5
sample_sd_reg <- 342.7
sample_mean_kiln <- 1875.2
sample_sd_kiln <- 332.9
df <- 20

# Compute the confidence interval for 𝜇reg - 𝜇kiln
t_value <- qt(0.995, df)
standard_error <- sqrt((sample_sd_reg^2/sample_size) + (sample_sd_kiln^2/sample_size))
margin_of_error <- t_value * standard_error
lower_bound <- round((sample_mean_reg - sample_mean_kiln) - margin_of_error)
upper_bound <- round((sample_mean_reg - sample_mean_kiln) + margin_of_error)

# Print the confidence interval
cat("99% Confidence Interval for 𝜇reg - 𝜇kiln:", lower_bound, "-", upper_bound, "\n")

# Part (b)
regular <- c(1903, 1935, 1910, 2496, 2108, 1961, 2060, 1444, 1612, 1316, 1511)
kiln_dried <- c(2009, 1915, 2011, 2463, 2180, 1925, 2122, 1482, 1542, 1443, 1535)
differences <- regular - kiln_dried

# Part (c)
sample_mean_diff <- round(mean(differences), 2)
cat("Sample Mean Difference, d:", sample_mean_diff, "\n")

# Part (d)
sample_sd_diff <- round(sd(differences), 2)
cat("Standard Deviation of Differences:", sample_sd_diff, "\n")

# Part (e)
t_value <- qt(0.995, length(differences) - 1)
standard_error_diff <- sample_sd_diff / sqrt(length(differences))
margin_of_error_diff <- t_value * standard_error_diff
lower_bound_diff <- round(sample_mean_diff - margin_of_error_diff, 2)
upper_bound_diff <- round(sample_mean_diff + margin_of_error_diff, 2)

# Print the confidence interval
cat("99% Confidence Interval for 𝜇d:", lower_bound_diff, "-", upper_bound_diff, "\n")

# Part (f)
cat("Since the standard error is smaller for the dependent data, the resulting confidence interval for 𝜇d is narrower than for 𝜇reg - 𝜇kiln.")