# Load the chickwts dataset
data(chickwts)

# Subset the dataset for the "casein" feed
casein <- chickwts[chickwts$feed == "casein", ]

# Subset the dataset for the "horsebean" feed
horsebean <- chickwts[chickwts$feed == "horsebean", ]

# (b) Construct a 95% confidence interval for the mean weight of chicks given the casein feed
casein_mean <- mean(casein$weight)
casein_sd <- sd(casein$weight)
n_casein <- length(casein$weight)
casein_se <- casein_sd / sqrt(n_casein)
casein_ci <- casein_mean + c(-1, 1) * qt(0.975, df = n_casein - 1) * casein_se
print("95% Confidence Interval for Casein feed:")
print(casein_ci)

# (c) Check plausibility of claim for casein feed
claim_casein <- 300
claim_plausible_casein <- claim_casein > casein_ci[1] & claim_casein < casein_ci[2]
print("Is the claim plausible for Casein feed?")
print(claim_plausible_casein)

# (d) Construct a 90% confidence interval for the mean weight of chicks given the horsebean feed
horsebean_mean <- mean(horsebean$weight)
horsebean_sd <- sd(horsebean$weight)
n_horsebean <- length(horsebean$weight)
horsebean_se <- horsebean_sd / sqrt(n_horsebean)
horsebean_ci <- horsebean_mean + c(-1, 1) * qt(0.95, df = n_horsebean - 1) * horsebean_se
print("90% Confidence Interval for Horsebean feed:")
print(horsebean_ci)

# (e) Check plausibility of claim for horsebean feed
claim_horsebean <- 185
claim_plausible_horsebean <- claim_horsebean > horsebean_ci[1] & claim_horsebean < horsebean_ci[2]
print("Is the claim plausible for Horsebean feed?")
print(claim_plausible_horsebean)