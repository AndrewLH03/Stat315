data(cars)

model <- lm(dist ~ speed, data = cars)
summary(model)

residuals <- residuals(model)

residual_stats <- c(min(residuals), quantile(residuals, probs = c(0.25, 0.5, 0.75)), max(residuals))

residual_table <- as.data.frame(t(data.frame(Statistic = c("Min", "Q1", "Median", "Q3", "Max"), Residual = residual_stats)))
colnames(residual_table) <- NULL
print(residual_table, digits = 4, quote = FALSE)

cat("\n\n")

coef_table <- data.frame(
    Estimate = coef(model),
    Std_Error = summary(model)$coefficients[, "Std. Error"],
    t_value = summary(model)$coefficients[, "t value"],
    PR_greater_t = summary(model)$coefficients[, "Pr(>|t|)"]
)
print(coef_table, digits = 4, na.print = "NA", quote = FALSE)

cat("\n\n")

par(mfrow = c(1, 2))  # Set the layout to have 1 row and 2 columns

plot(cars$speed, cars$dist, xlab = "Speed", ylab = "Distance", main = "Car Stopping Distance", cex = 2)
abline(model, col = "red", lwd = 2)

f_stat <- summary(model)$fstatistic[1]

cat("F-Statistic:", f_stat, "\n")
cat("P-Value:", coef_table$PR_greater_t[2], "\n")

alpha <- 0.05
df1 <- length(coef(model)) - 1
df2 <- length(residuals) - length(coef(model))
critical_f <- qf(1 - alpha, df1, df2)

x <- seq(0, 10, length.out = 1000)
y <- df(x, df1, df2)
plot(x, y, type = "l", xlab = "F-statistic", ylab = "Density", main = "F-distribution")
abline(v = f_stat, col = "red", lwd = 2)
abline(v = critical_f, col = "blue", lwd = 2)

legend("topright", legend = c("F-statistic", "Critical F-value"), col = c("red", "blue"), lwd = 2)
if (coef_table$PR_greater_t[2] < alpha) {
    cat("Reject H0: There is evidence of a linear relationship between speed and stopping distance.\n")
} else {
    cat("Fail to reject H0: There is no evidence of a linear relationship between speed and stopping distance.\n")
}
