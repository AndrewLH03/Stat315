# Load the chickwts dataset
data(chickwts)

# Subset the chicks that received "casein" feed and "horsebean" feed
casein <- chickwts[chickwts$feed == "casein", ]
horsebean <- chickwts[chickwts$feed == "horsebean", ]

# (b) Test statistic for H0: 𝜇casein - 𝜇horsebean = 0 vs. Ha: 𝜇casein - 𝜇horsebean ≠ 0
test_statistic_b <- t.test(casein$weight, horsebean$weight)$statistic

# (c) P-value for (b)
p_value_b <- t.test(casein$weight, horsebean$weight)$p.value

# (d) Decision of the hypothesis test for (b)
decision_b <- ifelse(p_value_b < 0.05, "Reject H0", "Fail to reject H0")

# (e) Test statistic for H0: 𝜇casein - 𝜇horsebean = 150 vs. Ha: 𝜇casein - 𝜇horsebean < 150
test_statistic_e <- t.test(casein$weight, horsebean$weight, alternative = "less", mu = 150)$statistic

# (f) P-value for (e)
p_value_e <- t.test(casein$weight, horsebean$weight, alternative = "less", mu = 150)$p.value

# (g) Decision of the hypothesis test for (e)
decision_e <- ifelse(p_value_e < 0.05, "Reject H0", "Fail to reject H0")

# (h) Test statistic for H0: 𝜇casein - 𝜇horsebean = 220 vs. Ha: 𝜇casein - 𝜇horsebean < 220
test_statistic_h <- t.test(casein$weight, horsebean$weight, alternative = "less", mu = 220)$statistic

# (i) P-value for (h)
p_value_h <- t.test(casein$weight, horsebean$weight, alternative = "less", mu = 220)$p.value

# (j) Decision of the hypothesis test for (h)
decision_h <- ifelse(p_value_h < 0.05, "Reject H0", "Fail to reject H0")

# Print the results
cat("(b) Test statistic:", test_statistic_b, "\n")
cat("(c) P-value:", p_value_b, "\n")
cat("(d) Decision:", decision_b, "\n\n")
cat("(e) Test statistic:", test_statistic_e, "\n")
cat("(f) P-value:", p_value_e, "\n")
cat("(g) Decision:", decision_e, "\n\n")
cat("(h) Test statistic:", test_statistic_h, "\n")
cat("(i) P-value:", p_value_h, "\n")
cat("(j) Decision:", decision_h, "\n")