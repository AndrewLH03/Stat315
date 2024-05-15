# Create an array of blood pressure values
blood_pressure <- c(110, 123, 135, 153, 129, 118, 101, 122)

# Print the array
print(blood_pressure[-which.min(blood_pressure)])

# Calculate median
median_value <- median(blood_pressure)

# Calculate quartiles
lower_quartile <- quantile(blood_pressure, 0.25)
upper_quartile <- quantile(blood_pressure, 0.75)

# Calculate interquartile range
interquartile_range <- upper_quartile - lower_quartile

# Output the results
cat("Median value: ", median_value, "\n")
cat("Lower quartile: ", lower_quartile, "\n")
cat("Upper quartile: ", upper_quartile, "\n")
cat("Interquartile range: ", interquartile_range, "\n\n")

# Calculate Q1, Q3, and IQR using computer calculated values
computer_q1 <- quantile(blood_pressure, 0.25)
computer_q3 <- quantile(blood_pressure, 0.75)
computer_iqr <- computer_q3 - computer_q1

# Calculate Q1, Q3, and IQR using hand calculated values
hand_q1 <- median(blood_pressure[blood_pressure < median_value])
hand_q3 <- median(blood_pressure[blood_pressure > median_value])
hand_iqr <- hand_q3 - hand_q1

# Create arrays for computer calculated values and hand calculated values
computer_values <- c(computer_q1, computer_q3, computer_iqr)
hand_values <- c(hand_q1, hand_q3, hand_iqr)

# Print the computer calculated values
cat("Computer calculated values:\n")
cat("Computer Q1: ", computer_q1, "\n")
cat("Computer Q3: ", computer_q3, "\n")
cat("Computer IQR: ", computer_iqr, "\n\n")

# Print the hand calculated values
cat("Hand calculated values:\n")
cat("Hand Q1: ", hand_q1, "\n")
cat("Hand Q3: ", hand_q3, "\n")
cat("Hand IQR: ", hand_iqr, "\n")