library(here)
source(paste0(here(), "/exercises/plot-save.R"))
title <- "10-unit-n-sphere"
set.seed(1234)

runif.esfera <- function(n, d) {
  M <- matrix(rnorm(n * d), nrow = n)
  L <- apply(M, 1,
             function(x) {
               sqrt(sum(x * x))
             })
  D <- diag(1 / L)
  return(D %*% M)
}

x <- runif.esfera(250, 2)
x <- data.frame(x)

p <- ggplot(x) +
  geom_point(aes(x = X1, y = X2),
             size = 0.1) +
  labs(title = "Unit circle",
       x = NULL,
       y = NULL,
       caption = "d = 2") +
  xlim(-1.5, 1.5) +
  theme_minimal()

plots <- list(p)

plot.save(title = title,
          plots = plots)
