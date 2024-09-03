# Sample information
n_younger <- 969
mean_younger <- 64.5
sd_younger <- 2.4

n_older <- 606
mean_older <- 63.1
sd_older <- 3.4

# (a) Calculate the confidence interval
z_critical <- qnorm(0.975)  # Two-sided 95% confidence level
se_diff <- sqrt((sd_younger^2 / n_younger) + (sd_older^2 / n_older))
margin_of_error <- z_critical * se_diff

lower_bound <- (mean_younger - mean_older) - margin_of_error
upper_bound <- (mean_younger - mean_older) + margin_of_error

# (b) Check the claim 𝜇younger - 𝜇older = 0
claim <- 0
if (claim >= lower_bound && claim <= upper_bound) {
    response <- "This claim is plausible since 0 is in the CI."
} else {
    response <- "This claim is implausible since 0 is not in the CI."
}

# (c) Hypothesis test
hypothesized_diff <- 1
test_statistic <- (mean_younger - mean_older - hypothesized_diff) / se_diff
p_value <- 1 - pnorm(test_statistic)

# (d) Conclusion of the hypothesis test
alpha <- 0.05
if (p_value < alpha) {
    conclusion <- "Reject H0 and conclude that younger women are, on average, at least one inch taller than older women."
} else {
    conclusion <- "Fail to reject H0 and conclude there is not sufficient evidence to support that younger women are, on average, at least one inch taller than older women."
}

# Print the results
cat("(a) The bounds for a two-sided 95% confidence interval for 𝜇younger - 𝜇older are", round(lower_bound, 2), "and", round(upper_bound, 2), "\n")
cat("(b) ", response, "\n")
cat("(c) The resulting test statistic is", round(test_statistic, 2), "and its p-value is", round(p_value, 2), "\n")
cat("(d) ", conclusion, "\n")