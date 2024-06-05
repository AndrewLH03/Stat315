# enter the data
x <- c(1.1, 2.2, 2.9, 4, 4.8)
y <- c(2.3, 4.6, 6.5, 7.1, 9.9)

# run the linear regression and see the results
model <- lm(y ~ x)
summary(model)

# Adding spaces before the residuals table
cat("\n\n")

# Outputting residuals in a table
residuals_table <- as.data.frame(t(data.frame(Residuals = resid(model))))
print(residuals_table)

# Adding spaces after the residuals table
cat("\n\n")

coefficients_table <- data.frame(
    Estimate = coef(model),
    Std.Error = summary(model)$coefficients[, 2],
    t.value = summary(model)$coefficients[, 3],
    Pr = summary(model)$coefficients[, 4]
)
print(coefficients_table, digits = 4, na.print = "NA", quote = FALSE)

# Adding spaces after the coefficients table
cat("\n\n")

# Adding comments
cat("The linear regression model is y =", round(coef(model)[1], 4), "+", round(coef(model)[2], 4), "* x\n")
cat("The coefficient of determination (R-squared) is", round(summary(model)$r.squared, 4), "\n")
cat("The p-value for the slope coefficient is", round(summary(model)$coefficients[2, 4], 4), "\n")

# Plotting the data points
plot(x, y, main = "Linear Regression", xlab = "x", ylab = "y")

# Adding the regression line to the plot
abline(model, col = "red")