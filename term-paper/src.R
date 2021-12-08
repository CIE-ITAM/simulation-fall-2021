library(tidyverse)
library(matlib)


data <- na.omit(read.csv("data/cancer.csv"))

sample.mean <- mean(data$DFS..in.months.)
sample.var <- var(data$DFS..in.months.)

mu.0 <- c(sample.mean, sample.var)

iters <- 1000

alpha.0 <- 20 # rnorm(1)
beta.0 <- 10 # runif(1)

theta <- matrix(NA, 2, iters)
theta[, 1] <- c(alpha.0, beta.0)

for (i in 2:iters) {
  n <- 2^i
  simulated <- rgamma(n, shape = theta[1, i - 1], scale = theta[2, i - 1])
  mu.hat <- c(mean(simulated), var(simulated)) # sum((simulated - mean(simulated))^2)
  mu.hat.1 <- matrix(0, 2, 2)
  u <- NULL
  h <- NULL

  for (j in 1:length(simulated)) {
    u[1] <- -digamma(theta[1, i - 1]) - log(theta[2, i - 1]) + log(simulated[j])
    u[2] <- (-theta[1, i - 1] / theta[2, i - 1]) + simulated[j] / (theta[2, i - 1]^2)
    h[1] <- simulated[j]
    h[2] <- (simulated[j] - mean(simulated))^2
    m <- h %*% t(u)
    mu.hat.1 <- mu.hat.1 + m
  }

  mu.hat.1 <- mu.hat.1 / length(simulated)

  par <- theta[, i - 1] + inv(mu.hat.1) %*% (mu.0 - mu.hat)

  if (par[1] * par[2] > 0) {
    theta[, i] <- par
  } else {
    theta[, i] <- theta[, i - 1] + runif(1)
  }

  print(i)
}

plot(theta[1,], type = "l")
plot(theta[2,], type = "l")

mean(theta[1,] * theta[2,])
mean(theta[1,] * (theta[2,]^2))
