library(here)
source(paste0(here(), "/exercises/plot-save.R"))
title <- "08-Poisson-sampling"
set.seed(1234)

k <- 10000

# generates a sample of 1000 Poisson variables with mean lambda = 3
lambda.1 <- 3
M <-  NULL

for (i in 1:k) {
    n <- 0
    P <- runif(1)
    while (P > exp(-lambda.1)) {
        u <- runif(1)
        P <- P * u
        n <- n + 1
    }
    M[i] <- n
}

M <- data.frame(M)

p.1 <- ggplot(M) +
    geom_histogram(
        aes(x = M, y = ..density..),
        binwidth = 0.25,
        fill = "#1e40ca",
        alpha = 0.8
    ) +
    labs(title = "Poisson Sampling",
         x = "x",
         caption = paste("mean =", lambda.1)) +
    theme_minimal()

# generates a sample of 1000 Poisson variables with mean lambda = 20
lambda.2 <- 20
z <- rnorm(k)

N <- ceiling(lambda.2 + sqrt(lambda.2) * z)

N <- data.frame(N)

p.2 <- ggplot(N) +
    geom_histogram(
        aes(x = N, y = ..density..),
        binwidth = 0.25,
        fill = "#1e40ca",
        alpha = 0.8
    ) +
    labs(title = "Poisson Sampling",
         x = "x",
         caption = paste("mean =", lambda.2)) +
    theme_minimal()

plots <- list(p.1, p.2)

plot.save(title = title,
          plots = plots)
