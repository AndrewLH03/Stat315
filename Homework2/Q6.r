set.seed(2020)
# Simulating rolling 100 fair, six-sided dice
data = sample(x = 1:6, size = 100, replace = TRUE)
data

n1 = sum(data == 1);