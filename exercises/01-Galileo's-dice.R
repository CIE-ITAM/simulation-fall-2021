# How long does it take to observe that
# 10 is more common than 9 with three dice?

source(paste0(here(), "/exercises/plot-save.R"))
title <- "01-Galileo's-dice"
set.seed(1234)

# Parameters
N      <- 5000 # number of simulations
count  <- rep(0, 18)
probs  <- rep(0, 18)

# Simulation
die1 <- sample(1:6, N, replace = TRUE) # die 1
die2 <- sample(1:6, N, replace = TRUE) # die 2
die3 <- sample(1:6, N, replace = TRUE) # die 3

dieSum <- die1 + die2 + die3 # sum of three dice

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

# Gr?ficas

p.1 <- ggplot(df.1, aes(x = number, y = probs)) +
    geom_bar(stat = "identity", fill = "black") +
    labs(title = "Galileo's dice",
         x = "sum of the dice",
         y = "prob") +
    theme_minimal()

p.2 <- ggplot(df.2, aes(x = sim, y = prob, colour = number)) +
    geom_line() +
    scale_color_manual(values = c("#1e40ca", "black"),
                       labels = c("9", "10")) +
    labs(
        title = "Galileo's dice \nSimulation",
        x = "number of simulations",
        y = "prob",
        colour = "number"
    ) +
    ylim(0, 1) +
    theme_minimal()

plots <- list(p.1, p.2)

plot.save(title = title,
          plots = plots)
