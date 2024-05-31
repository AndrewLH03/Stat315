# Declaring Variables
sampleSize <- 20
meanStudents <- 78.5
meanProfessor <- 80
popVariance <- 119

# Calculate the z-value using the formula (meanStudents - meanProfessor) / (sqrt(popVariance / sampleSize))
z_value <- (meanStudents - meanProfessor) / (sqrt(popVariance / sampleSize))

# Calculate the p-value using the pt() function with the z-value, sampleSize - 1 degrees of freedom, and lower.tail = TRUE
p_value <- pt(z_value, sampleSize - 1, lower.tail = TRUE) * 2

# Print the calculated z-value
cat("Z value:", z_value, "\n")

# Print the calculated p-value
cat("P value:", p_value, "\n")

# Conduct hypothesis test at alpha = 0.05
alpha <- 0.01
if (p_value < alpha) {
    cat("Reject the null hypothesis")
} else {
    cat("Fail to reject the null hypothesis")
}