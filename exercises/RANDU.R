library(tidyverse)
options(scipen = 9)

# Generador RANDU
seed <- as.double(1)

RANDU <- function() {
    seed <<-
        ((2 ^ 16 + 3) * seed) %% (2 ^ 31) # Cambia la semilla globalmente
    seed / (2 ^ 31)
}

randu <- NULL
for (i in 1:1000)
    randu[i] <- RANDU()

df <- data.frame(x = randu[2:length(randu)], y = randu[1:length(randu) - 1])

p <- ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    theme_minimal() +
    labs(title = 'Generador RANDU',
         x = expression(u[i]),
         y = expression(u[i - 1]))

ggsave(path = 'simulation2021/exercises/',
       filename = 'RANDU.svg',
       plot = p,
       width = 15,
       height = 10)