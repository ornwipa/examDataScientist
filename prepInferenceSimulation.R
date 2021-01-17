##### Central Limit Theorem #####
# Standard Error
x_hat <- 0.48
se <- sqrt(x_hat * (1 - x_hat) / 25)
1.96*se
# [1] 0.1958431
pnorm(1.96)-pnorm(-1.96) # used for significant level & p-value
# [1] 0.9500042
qnorm(0.975) # convert probability back to z-value
# [1] 1.959964

# Monte Carlo Simulation
p <- 0.45
B <- 10000
N <- 1000
x_hat <- replicate(B, {
  x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})
qqnorm(x_hat); qqline(x_hat) # create normal Q-Q plot

##### Confidence Intervals #####
# Standard Error
p <- 0.45
x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1 - x_hat) / N)
c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
# [1] 0.4102262 0.4717738

# Monte Carlo Simulation
p <- 0.45
B <- 10000
N <- 1000
inside <- replicate(B, {
  x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  between(p, x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
})
mean(inside)
# [1] 0.9507
