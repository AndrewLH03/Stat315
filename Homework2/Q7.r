set.seed(2020)
d1 <- sample(x = 1:6, size = 10000, replace = TRUE)
d2 <- sample(x = 1:6, size = 10000, replace = TRUE)

d3 <- d1 + d2

# (a) Most common value for the sum of the two dice
most_common <- names(table(d3))[which.max(table(d3))]
print(paste("Most common value for the sum of the two dice (d3):", most_common))

# (b) Barplot of the distribution of the sum of the two dice
barplot(table(d3), main = "Distribution of the sum of the two dice")

# (c) Sample mean of the two dice
mean_d3 <- mean(d3)
print(paste("Sample mean of the two dice:", mean_d3))

# (d) Sample variance of the two dice
var_d3 <- var(d3)
print(paste("Sample variance of the two dice:", var_d3))

# (e) Probability that the sum of the two dice (d3) is less than 5
prob_less_than_5 <- sum(d3 < 5) / length(d3)
print(paste("Probability that the sum of the two dice (d3) is less than 5:", prob_less_than_5))

# (f) Probability that the first die (d1) is 6 and the second die (d2) is greater than 3
prob_d1_6_d2_gt_3 <- sum(d1 == 6 & d2 > 3) / length(d3)
print(paste("Probability that the first die (d1) is 6 and the second die (d2) is greater than 3:", prob_d1_6_d2_gt_3))