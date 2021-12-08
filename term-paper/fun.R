library(tidyverse)
library(matlib)

theme_set(theme_minimal())

gamma.moments <- function(data, iters, alpha.0, beta.0, fun) {
  sample.mean <- mean(data)
  sample.var <- var(data)
  mu.0 <- c(sample.mean, sample.var)
  theta <- matrix(NA, 2, iters)
  theta[, 1] <- c(alpha.0, beta.0)

  for (i in 2:iters) {
    n <- do.call(fun, list(i))
    simulated <- rgamma(n,
                        shape = theta[1, i - 1],
                        scale = theta[2, i - 1])
    mu <- c(mean(simulated), var(simulated))
    mu.hat <- matrix(0, 2, 2)
    h <- u <- NULL

    for (j in 1:length(simulated)) {
      u[1] <- -digamma(theta[1, i - 1]) - log(theta[2, i - 1]) + log(simulated[j])
      u[2] <- (-theta[1, i - 1] / theta[2, i - 1]) + simulated[j] / (theta[2, i - 1]^2)
      h[1] <- simulated[j]
      h[2] <- (simulated[j] - mean(simulated))^2
      m <- h %*% t(u)
      mu.hat <- mu.hat + m
    }

    mu.hat <- mu.hat / length(simulated)

    par <- theta[, i - 1] + inv(mu.hat) %*% (mu.0 - mu)

    if (par[1] * par[2] > 0) {
      theta[, i] <- par
    } else {
      theta[, i] <- theta[, i - 1] + runif(1)
    }
  }

  theta <- data.frame(x = theta[1,],
                      y = theta[2,],
                      n = 1:iters)

  p.1 <- ggplot(theta) +
    geom_line(aes(x = n, y = x),
              size = 0.1) +
    labs(title = NULL,
         x = "Iteración",
         y = expression(alpha))

  p.2 <- ggplot(theta) +
    geom_line(aes(x = n, y = y),
              size = 0.1) +
    labs(title = NULL,
         x = "Iteración",
         y = expression(beta))

  shape <- mean(theta$x)
  scale <- mean(theta$y)

  dist.mean <- mean(shape * scale)
  dist.var <- mean(shape * (scale^2))

  results <- list(
    p.1, p.2, shape, scale, dist.mean, dist.var, sample.mean, sample.var
  )

  return(results)
}

fun.1 <- function(x) {
  100 + x
}

df <- na.omit(read.csv("data/cancer.csv"))
data <- df$DFS..in.months.

test.1 <- gamma.moments(data, 1000, 20, 10, fun.1)
test.1[[1]]
test.1[[2]]
test.1[[3]]
test.1[[4]]
test.1[[5]]
test.1[[6]]
test.1[[7]]
test.1[[8]]
