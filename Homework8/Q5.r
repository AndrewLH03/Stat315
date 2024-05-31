data(chickwts)

# Selecting data for "casein" feed
casein <- chickwts[chickwts$feed == "casein", ]
print("Casein data:")
print(casein)

# Selecting data for "horsebean" feed
horsebean <- chickwts[chickwts$feed == "horsebean", ]
print("Horsebean data:")
print(horsebean)

# Declaring Variables
caseinSampleSize = nrow(casein)
caseinTrueMean = 300
caseinSampleMean = sum(casein$weight) / caseinSampleSize
alpha = 0.05

# Calculating t-value
t_value = (caseinSampleMean - caseinTrueMean) / (sd(casein$weight) / sqrt(caseinSampleSize))

# Calculating p-value
p_value = pt(t_value, caseinSampleSize - 1, lower.tail = FALSE) * 2

# Printing the t-value and p-value
cat("\n\nTesting CaseIn Mean as ", caseinTrueMean)
cat("\nt_value:", t_value, "\n")
cat("p_value:", p_value, "\n")

#----------------------------------------------

# Declaring Variables
caseinTrueMean = 310
alpha = 0.01

# Calculating t-value
t_value = (caseinSampleMean - caseinTrueMean) / (sd(casein$weight) / sqrt(caseinSampleSize))

# Calculating p-value
p_value = pt(t_value, caseinSampleSize - 1, lower.tail = FALSE)

# Printing the t-value and p-value
cat("\n\nTesting CaseIn Mean as ", caseinTrueMean)
cat("\nt_value:", t_value, "\n")
cat("p_value:", p_value, "\n")

#----------------------------------------------

# Declaring Variables
horsebeanSampleSize = nrow(horsebean)
horsebeanTrueMean = 130
horsebeanSampleMean = sum(horsebean$weight) / horsebeanSampleSize
alpha = 0.10

# Calculating t-value
t_value = (horsebeanSampleMean - horsebeanTrueMean) / (sd(horsebean$weight) / sqrt(horsebeanSampleSize))

# Calculating p-value
p_value = pt(t_value, horsebeanSampleSize - 1, lower.tail = FALSE) * 2

# Printing the t-value and p-value
cat("\n\nTesting HorseBean Mean as ", horsebeanTrueMean)
cat("\nt_value:", t_value, "\n")
cat("p_value:", p_value, "\n/n")

