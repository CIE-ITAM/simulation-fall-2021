library(here)
source(paste0(here(), "/exercises/plot-save.R"))
title <- "04-inverse-transform-sampling"
set.seed(1234)

n <- 10000

# Erlang Distribution
sample.1 <- NULL
alpha <- 10
beta <- 2

for (i in 1:n) {
  sample.1[i] <- -beta * sum(log(runif(alpha)))
}

dist.1 <- dgamma(sample.1, shape = alpha, scale = beta)

df.1 <- data.frame(sample.1, dist.1)

p.1 <- ggplot(df.1, aes(x = sample.1, y = ..density..)) +
  geom_histogram(bins = 20, fill = "#00a2ed", alpha = 0.6) +
  geom_line(aes(x = sample.1, y = dist.1), color = "#1e40ca") +
  theme_minimal() +
  labs(title = "Erlang Distribution",
       x = "x",
       y = "density")

# Cauchy Distrubution
sample.2 <- NULL

for (i in 1:n) {
  sample.2[i] <- tan(pi * (runif(1) - 1 / 2))
}

dist.2 <- dcauchy(sample.2)

df.2 <- data.frame(sample.2, dist.2)

p.2 <- ggplot(df.2, aes(x = sample.2, y = ..density..)) +
  geom_histogram(bins = 20, fill = "#00a2ed", alpha = 0.6) +
  geom_line(aes(x = sample.2, y = dist.2), color = "#1e40ca") +
  xlim(-5, 5) +
  theme_minimal() +
  labs(title = "Cauchy Distrubution",
       x = "x",
       y = "density")

# Poisson Distribution
fn.3 <- F.3 <- NULL
lambda <- 2

fn.3[1] <- exp(-lambda)
F.3[1] <- fn.3[1]

for (i in 2:(n + 1)) {
  fn.3[i] <- lambda * fn.3[i - 1] / (i - 1)
  F.3[i] <- F.3[i - 1] + fn.3[i]
}

u <- runif(n)

sample.3 <- NULL

for (i in 1:n) {
  sample.3 <- append(sample.3, sum(F.3 < u[i]))
}

dist.3 <- rpois(n, lambda)

df.3 <- data.frame(sample.3, dist.3)

p.3 <- ggplot(df.3, aes(x = sample.3, y = ..density..)) +
  geom_histogram(binwidth = 0.25,
                 fill = "#00a2ed",
                 alpha = 0.6) +
  geom_histogram(
    aes(x = dist.3, y = ..density..),
    binwidth = 0.25,
    fill = "#1e40ca",
    alpha = 0.6
  ) +
  theme_minimal() +
  labs(title = "Poisson Distribution",
       x = "x",
       y = "density")

# Laplace Distribution

u <- matrix(runif(n), ncol = 2)

sample.4 <- ifelse(u[, 1] < 0.5, -1, 1) * log(u[, 2])
dist.4 <- 0.5 * exp(ifelse(sample.4 < 0, 1, -1) * sample.4)
df.4 <- data.frame(sample.4, dist.4)

p.4 <- ggplot(df.4, aes(x = sample.4, y = ..density..)) +
  geom_histogram(bins = 20, fill = "#00a2ed", alpha = 0.6) +
  geom_line(aes(x = sample.4, y = dist.4), color = "#1e40ca") +
  xlim(-5, 5) +
  theme_minimal() +
  labs(title = "Laplace Distribution",
       x = "x",
       y = "density")

# Mixture of Normal Distributions

gen <- sample(1:3,
              size = n,
              prob = c(0.1, 0.7, 0.2),
              replace = T)
sample.5 <- rnorm(n,
                  mean = ifelse(gen == 1, 1, ifelse(gen == 2, 2, 5)),
                  sd = ifelse(gen == 1, 0.01, ifelse(gen == 2, 0.5, 1)))
dist.5 <- 0.1 * dnorm(sample.5, 1, sd = 0.1) +
  0.7 * dnorm(sample.5, 2, sd = 0.5) +
  0.2 * dnorm(sample.5, 5, sd = 1)

df.5 <- data.frame(sample.5, dist.5)

p.5 <- ggplot(df.5, aes(x = sample.5, y = ..density..)) +
  geom_histogram(bins = 50, fill = "#00a2ed", alpha = 0.6) +
  geom_line(aes(x = sample.5, y = dist.5), color = "#1e40ca") +
  theme_minimal() +
  labs(title = "Mixture of Normal Distributions",
       x = "x",
       y = "density")

plots <- list(p.1, p.2, p.3, p.4, p.5)

plot.save(title = title,
          plots = plots)
