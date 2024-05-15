data(Nile)
?Nile

cat("Nile data set:\n")
print(format(Nile, scientific = FALSE))

sample_mean <- mean(Nile)
sample_variance <- var(Nile)
sample_standard_deviation <- sd(Nile)

cat("Sample mean:", sample_mean, "\n")
cat("Sample variance:", sample_variance, "\n")
cat("Sample standard deviation:", sample_standard_deviation, "\n")