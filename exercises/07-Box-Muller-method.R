library(here)
source(paste0(here(), "/exercises/plot-save.R"))
title <- "07-Box-Muller-method"
set.seed(1234)

# see polar method for more efficiency

seed <- 10
n <- 10000

BM <- function(u) {
  R <- sqrt(-2 * log(u[1]))
  th <- 2 * pi * u[2]
  z <- R * c(cos(th), sin(th))
  return(z)
}

z <- matrix(0, nrow = n, ncol = 2)

for (i in 1:n) {
  z[i,] <- BM(c(runif(1), runif(1)))
}

z <- data.frame(z)

p.1 <- ggplot(z, aes(x = X1)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 fill = "#00a2ed",
                 alpha = 0.6) +
  theme_minimal() +
  labs(title = "Box-Muller Method",
       x = expression(u[1]),
       caption = paste("n =", n))

p.2 <- ggplot(z, aes(x = X2)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 fill = "#00a2ed",
                 alpha = 0.6) +
  theme_minimal() +
  labs(title = "Box-Muller Method",
       x = expression(u[2]),
       caption = paste("n =", n))

p.3 <- ggplot(z) +
  geom_point(aes(x = X1, y = X2), size = 0.2) +
  theme_minimal() +
  labs(
    title = "Box-Muller Method",
    x = expression(u[1]),
    y = expression(u[2]),
    caption = paste("n =", n)
  )

plots <- list(p.1, p.2, p.3)

plot.save(title = title,
          plots = plots)
