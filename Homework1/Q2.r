# Import the necessary package for the print function
library(base)

# Create the vector for impact strength
impact_strength <- c(43, 48, 41, 48, 60)

# Calculate the mean
mean_value <- mean(impact_strength)

# Calculate the deviation from the mean for each value
deviation <- impact_strength - mean_value

# Calculate the deviation squared
deviation_squared <- deviation^2

# Calculate the variance
variance <- var(impact_strength)

# Calculate the standard deviation
standard_deviation <- sqrt(variance)

# Create a data frame with impact strength, deviation, deviation squared, variance, and standard deviation
impact_df <- data.frame(impact_strength, deviation, deviation_squared, variance, standard_deviation)

# Label each column within the impact_df data frame
colnames(impact_df) <- c("Impact Strength", "Deviation", "Deviation Squared", "Variance", "Standard Deviation")

# Calculate the sum of squares
sum_of_squares <- sum(deviation_squared)

# Calculate the deviation for the fifth observation
deviation_5th <- impact_strength[5] - mean_value

# Calculate the z-score for the fourth observation
z_score_4th <- (impact_strength[4] - mean_value) / standard_deviation

# Print the sum of squares with units
cat("Sum of Squares: ", sum_of_squares, " (foot-pound/in^2)\n")

# Print the variance with units
cat("Variance: ", variance, " (foot-pound/in^2)\n")

# Print the standard deviation with units
cat("Standard Deviation: ", standard_deviation, " (foot-pound/in)\n")

# Print the deviation for the fifth observation with units
cat("Deviation for the fifth observation: ", deviation_5th, " (foot-pound/in)\n")

# Print the z-score for the fourth observation
cat("Z-score for the fourth observation: ", z_score_4th, "\n")

# Print the data frame with extra spacing between columns
print(impact_df, sep = "\t")

# Convert impact_strength from foot-pound/in to J/m
impact_strength <- impact_strength * 53.35

# Calculate the mean of the converted impact_strength
mean_value_new <- mean(impact_strength)

# Calculate the deviation from the mean for each value in the converted impact_strength
deviation_new <- impact_strength - mean_value_new

# Calculate the deviation squared for the converted impact_strength
deviation_squared_new <- deviation_new^2

# Calculate the variance of the converted impact_strength
variance_new <- var(impact_strength)

# Calculate the standard deviation of the converted impact_strength
standard_deviation_new <- sqrt(variance_new)

# Calculate the z-score for the fourth transformed observation
z_score_4th_transformed <- (impact_strength[4] - mean_value_new) / standard_deviation_new

# Print the mean of the converted impact_strength with units
cat("Mean of converted impact strength: ", mean_value_new, " (J/m)\n")

# Print the variance of the converted impact_strength with units
cat("Variance of converted impact strength: ", variance_new, " (J/m)\n")

# Print the standard deviation of the converted impact_strength with units
cat("Standard deviation of converted impact strength: ", standard_deviation_new, " (J/m)\n")

# Print the z-score for the fourth transformed observation
cat("Z-score for the fourth transformed observation: ", z_score_4th_transformed, "\n")