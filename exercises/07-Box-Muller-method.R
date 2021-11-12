source(paste0(here(), "/exercises/plot-save.R"))
title <- "07-Box-Muller-method"
set.seed(1234)

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
                   fill = "#00a2ed") +
    theme_minimal() +
    labs(title = "Box-Muller method",
         x = expression(u[1]),
         caption = paste("n =", n))

p.2 <- ggplot(z, aes(x = X2)) +
    geom_histogram(aes(y = ..density..),
                   bins = 20,
                   fill = "#00a2ed") +
    theme_minimal() +
    labs(title = "Box-Muller method",
         x = expression(u[2]),
         caption = paste("n =", n))

p.3 <- ggplot(z) +
    geom_point(aes(x = X1, y = X2), size = 0.2) +
    theme_minimal() +
    labs(
        title = "Box-Muller method",
        x = expression(u[1]),
        y = expression(u[2]),
        caption = paste("n =", n)
    )

RANDU <- function() {
    seed <<- (3 * seed + 1) %% (64)
    seed / (64)
}

u <- NULL

for (i in 1:n) {
    u <- c(u, RANDU())
}

R <- sqrt(-2 * log(u[1:(n - 1)]))
th <- u[2:n]
z1 <- R * cos(2 * pi * th)
z2 <- R * sin(2 * pi * th)

par(
    mfrow = c(2, 2),
    oma = c(1, 1, 1, 1) + 0.1,
    mar = c(1, 1, 1, 1) + 0.1
)
plot(u[1:(n - 1)], u[2:n], pch = 16, cex = 0.4)
plot(z1, z2, pch = 16, cex = 0.2)
v <- sample(u)
R <- sqrt(-2 * log(v[1:(n - 1)]))
th <- v[2:n]
plot(v[1:(n - 1)], v[2:n], pch = 16, cex = 0.4)
plot(R * cos(2 * pi * th),
     R * sin(2 * pi * th),
     pch = 16,
     cex = 0.2)

plots <- list(p.1, p.2, p.3)

plot.save(title = title,
          plots = plots)
