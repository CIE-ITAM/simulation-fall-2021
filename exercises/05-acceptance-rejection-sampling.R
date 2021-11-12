source(paste0(here(), "/exercises/plot-save.R"))
title <- "05-acceptance-rejection-sampling"
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

p.1 <- ggplot(df.1) +
    # geom_line(aes(x = x, y = f.x.), colour = "#1e40ca") +
    geom_line(aes(x = x, y = g.x.), colour = "black", alpha = 0.8) +
    geom_line(aes(x = x, y = c * g.x.), colour = "#1e40ca", alpha = 0.8) +
    geom_point(data = df.2, colour = "#00a2ed",
               aes(x = w, y = u1.indicadora....c...g.w.),
               size = 0.05) +
    theme_minimal() +
    labs(title = "Acceptance-Rejection Sampling",
         x = "x",
         y = "y")

plots <- list(p.1)

plot.save(title = title,
          plots = plots)

table(indicadora)/n

1/c # accepted points
