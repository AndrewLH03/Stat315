# load the data
data(cars)

# run the regression and summarize the results
model <- lm(dist ~ speed, data = cars)
summary(model)

# Extract the residuals
residuals <- residuals(model)

# Calculate the desired statistics
residual_stats <- c(min(residuals), quantile(residuals, probs = c(0.25, 0.5, 0.75)), max(residuals))

# Create a table for residuals
residual_table <- as.data.frame(t(data.frame(Statistic = c("Min", "Q1", "Median", "Q3", "Max"), Residual = residual_stats)))
colnames(residual_table) <- NULL
print(residual_table, digits = 4, quote = FALSE)

cat("\n\n")

# Create a table for coefficients
coef_table <- data.frame(
    Estimate = coef(model),  # Coefficient estimates
    Std_Error = summary(model)$coefficients[, "Std. Error"],  # Standard errors
    t_value = summary(model)$coefficients[, "t value"],  # t-values
    PR_greater_t = summary(model)$coefficients[, "Pr(>|t|)"]  # p-values
)
print(coef_table, digits = 4, na.print = "NA", quote = FALSE)

cat("\n\n")

# Plot the data and line of best fit
plot(cars$speed, cars$dist, xlab = "Speed", ylab = "Distance", main = "Car Stopping Distance")
abline(model, col = "red")