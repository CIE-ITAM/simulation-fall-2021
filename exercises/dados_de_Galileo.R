library(ggplot2)
library(tidyr)

set.seed(1234)

# Parámetros
N      <- 5000 # Número de simulaciones
count  <- rep(0, 18)
probs  <- rep(0, 18)

# Simulación
die1 <- sample(1:6, N, replace = TRUE) # dado 1
die2 <- sample(1:6, N, replace = TRUE) # dado 2
die3 <- sample(1:6, N, replace = TRUE) # dado 3

dieSum <- die1 + die2 + die3 # suma de los tres dados

for (i in 1:18) {
    count[i] <- sum(dieSum == i)
}

probs <- count / N

prob.09 <- ifelse(dieSum == 9, 1, 0)
prob.09 <- cumsum(prob.09) / 1:N

prob.10 <- ifelse(dieSum == 10, 1, 0)
prob.10 <- cumsum(prob.10) / 1:N

df.1 <- data.frame(number = 1:18, probs)
df.2 <- data.frame(sim = 1:N, nine = prob.09, ten = prob.10)
df.2 <- df.2 %>% gather(number, prob, nine, ten)

# Gráficas

p.1 <- ggplot(df.1, aes(x = number, y = probs)) +
    geom_bar(stat = 'identity', fill = 'black') +
    labs(title = 'Dados de Galileo',
         x = 'Suma de los dados',
         y = 'Probabilidad') +
    theme_minimal()

p.2 <- ggplot(df.2, aes(x = sim, y = prob, colour = number)) +
    geom_line() +
    scale_color_manual(values = c('tomato2', 'black'),
                       labels = c('9', '10')) +
    labs(title = 'Dados de Galileo \nSimulación',
         x = 'Número de simulaciones',
         y = 'Probabilidad',
         colour = 'Número') +
    ylim(0, 1) +
    theme_minimal()

ggsave(path = 'simulation2021/exercises/',
       filename = 'dados_de_Galileo_1.svg',
       plot = p.1,
       width = 15,
       height = 10)

ggsave(path = 'simulation2021/exercises/',
       filename = 'dados_de_Galileo_2.svg',
       plot = p.2,
       width = 15,
       height = 10)
