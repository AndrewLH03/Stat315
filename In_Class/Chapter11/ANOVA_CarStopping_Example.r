# Load the cars dataset
data(cars)

# Create a linear regression model
model <- lm(dist ~ speed, data = cars) 

# Print the summary of the model
summary(model)

# Perform ANOVA on the model
anova_table <- anova(model)

# Print the ANOVA table with definitive lines between columns
print(anova_table, justify = "none", sep = "|")