library(here)
library(MASS)
source(paste0(here(), "/exercises/plot-save.R"))
title <- "11-NORTA"
set.seed(1234)

n <- 10000
corr <- 0.7
mu <- rep(0, 4)
sigma <- matrix(corr, nrow = 4, ncol = 4) + diag(4) * 0.3
z <- mvrnorm(n = n, mu = mu, Sigma = sigma)

# Normal distributions
X <- data.frame(x = z[, 1], y = z[, 2])

p.1 <- ggplot(X) +
    geom_point(aes(x = x, y = y), size = 0.1) +
    theme_minimal() +
    labs(title = "NORTA",
         caption = "Normal distributions")

# Uniform distributions
u <- qunif(pnorm(z))
X <- data.frame(x = u[, 1], y = u[, 2])

p.2 <- ggplot(X) +
    geom_point(aes(x = x, y = y), size = 0.1) +
    theme_minimal() +
    labs(title = "NORTA",
         caption = "Uniform distributions")

# Poisson distributions
pois <- qpois(pnorm(z), lambda = 5)
X <- data.frame(x = pois[, 1], y = pois[, 2])

p.3 <- ggplot(X) +
    geom_point(aes(x = x, y = y), size = 0.5) +
    theme_minimal() +
    labs(title = "NORTA",
         caption = "Poisson distributions")

plots <- list(p.1, p.2, p.3)

plot.save(title = title,
          plots = plots)
