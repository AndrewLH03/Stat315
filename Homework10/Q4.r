# Define the data for each group
colorado = c(15, 18, 17, 16, 16, 15)
montana = c(18, 19, 16, 20, 18, 19)
alberta = c(21, 25, 26, 23, 27, 25)

# Define the total number of observations and groups
N = 18
k = 3

# Calculate the mean for each group
coloradoMean = mean(colorado)
montanaMean = mean(montana)
albertaMean = mean(alberta)

# Calculate the variance for each group
coloradoVar = var(colorado)
montanaVar = var(montana)
albertaVar = var(alberta)

dfTR = k - 1
dfE = N - k
df = N - 1

# Calculate the sum of squares between groups (SSTR)
SSTR = length(colorado) * (coloradoMean - mean(c(colorado, montana, alberta)))^2 + 
    length(montana) * (montanaMean - mean(c(colorado, montana, alberta)))^2 + 
    length(alberta) * (albertaMean - mean(c(colorado, montana, alberta)))^2

# Calculate the sum of squares within groups (SSE)
SSE = (length(colorado) - 1) * coloradoVar + (length(montana) - 1) * montanaVar + 
    (length(alberta) - 1) * albertaVar

# Calculate the total sum of squares (SS)
SS = SSTR + SSE

# Calculate the mean sum of squares between groups (MSTR)
MSTR = SSTR/(k - 1)

# Calculate the mean sum of squares within groups (MSE)
MSE = SSE/(N - k)

# Calculate the F-test statistic
Ftest = MSTR / MSE

# Create a table to display the information
groups <- data.frame(
    Group = c("Colorado", "Montana", "Alberta"),
    Mean = c(coloradoMean, montanaMean, albertaMean),
    Variance = c(coloradoVar, montanaVar, albertaVar)
)

ANOVA <- data.frame(
    SSTR = SSTR,
    SSE = SSE,
    SS = SS,
    MSTR = MSTR,
    MSE = MSE,
    Ftest = Ftest,
    dfTR = dfTR,
    dfE = dfE,
    df = df
)

print(output)

print(ANOVA)

# Assuming you have already calculated the means and variances for each group

# Create a data frame with the group names, means, and variances
data <- data.frame(
  Group = c("Colorado", "Montana", "Alberta"),
  Mean = c(coloradoMean, montanaMean, albertaMean),
  Variance = c(coloradoVar, montanaVar, albertaVar)
)

# Perform ANOVA and store the output
ANOVA <- aov(Mean ~ Group, data = data)

# Perform Tukey HSD test
tukey_result <- TukeyHSD(ANOVA)

# Print the Tukey HSD results
print(tukey_result)

