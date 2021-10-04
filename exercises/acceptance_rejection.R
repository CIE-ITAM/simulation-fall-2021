# Método de aceptación y rechazo
library(tidyverse)

set.seed(1234)

n <- 10000
alpha <- 4

x <- seq(0, 25, 0.1)
f <- function(x) {
    dgamma(x, alpha, 1)
}
g <- function(x) {
    dexp(x, 1 / alpha)
}

v <- optimize(
    f = function(x) {
        f(x) / g(x)
    },
    interval = c(0, 25),
    maximum = T
)

c <- v$objective

u1 <- runif(n)
u2 <- runif(n)

t <- -alpha * log(1 - u2)

indicadora <- u1 <= f(t) / (c * g(t))
w <- t[indicadora]

df.1 <- data.frame(x, f(x), g(x))
df.2 <- data.frame(w, u1[indicadora] * c * g(w))

p <- ggplot(df.1) +
    geom_line(aes(x = x, y = f.x.)) +
    geom_line(aes(x = x, y = g.x.), colour = '#2d6c5f', alpha = 0.8) +
    geom_line(aes(x = x, y = c * g.x.), colour = '#2d6c5f', alpha = 0.8) +
    geom_point(data = df.2, aes(x = w, y = u1.indicadora....c...g.w.), size = 0.05) +
    theme_minimal() +
    labs(title = 'Mezcla de distribuciones normales',
         x = 'x',
         y = 'y')

ggsave(
    path = 'simulation2021/exercises/',
    filename = 'acceptance_rejection.svg',
    plot = p,
    width = 15,
    height = 10
)

table(indicadora)/n

1/c # puntos aceptados
