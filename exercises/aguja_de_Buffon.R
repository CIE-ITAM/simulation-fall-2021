# Estima la probabilidad de que una aguja cruce una línea en un piso de duela
library(ggplot2)

set.seed(1234)

# Parámetros
N     <- 2000                 # Número de simulaciones
alpha <- 0.05                 # Nivel de significancia
z     <- qnorm(1 - alpha / 2) # Cuantil de la distribución normal
NI    <- rep(1, N)            # Agujas que tocan el límite de la duela
r     <- 10                   # Longitud de la aguja
d     <- 15                   # Ancho de la duela

# Simulación
u1      <- runif(N, min = 0, max = d / 2)
u2      <- runif(N, min = 0, max = pi / 2)
prob    <- ifelse(r / 2 * sin(u2) >= u1, 1, 0)
prob    <- cumsum(prob) / 1:N
lim.inf <- prob - z * sqrt(prob * (1 - prob) / 1:N)
lim.sup <- prob + z * sqrt(prob * (1 - prob) / 1:N)

df <- data.frame(sim = 1:N, prob, lim.inf, lim.sup)

# Gráfica
p <- ggplot(df, aes(x = sim, y = prob)) +
    geom_line(colour = 'black') +
    geom_ribbon(aes(ymin = lim.inf, ymax = lim.sup), alpha = 0.1) +
    geom_hline(yintercept = 2 * r / (pi * d)) +
    labs(title = 'Aguja de Buffon \nSimulación',
         x = 'Número de simulaciones',
         y = 'Probabilidad') +
    ylim(0, 1) +
    theme_minimal()

ggsave(path = 'simulation2021/exercises/',
       filename = 'aguja_de_Buffon.svg',
       plot = p,
       width = 15,
       height = 10)
