# Buffon's needle problem

# Suppose we have a floor made of parallel strips of wood, each the same width,
# and we drop a needle onto the floor. What is the probability that the needle
# will lie across a line between two strips?

source(paste0(here(), "/exercises/plot-save.R"))
title <- "00-Buffon's-needle"
set.seed(1234)

# Parameters
N     <- 2000                 # number of simulations
alpha <- 0.05                 # significance level
z     <- qnorm(1 - alpha / 2) # quantile of the normal distribution
NI    <- rep(1, N)            # needles that cross a line
r     <- 10                   # needle length
d     <- 15                   # width of the strip

# Simulation
u1      <- runif(N, min = 0, max = d / 2)
u2      <- runif(N, min = 0, max = pi / 2)
prob    <- ifelse(r / 2 * sin(u2) >= u1, 1, 0)
prob    <- cumsum(prob) / 1:N
lim.inf <- prob - z * sqrt(prob * (1 - prob) / 1:N)
lim.sup <- prob + z * sqrt(prob * (1 - prob) / 1:N)

df <- data.frame(sim = 1:N, prob, lim.inf, lim.sup)

# Plot
p.1 <- ggplot(df, aes(x = sim, y = prob)) +
    geom_line(colour = "black") +
    geom_ribbon(aes(ymin = lim.inf, ymax = lim.sup), alpha = 0.1) +
    geom_hline(yintercept = 2 * r / (pi * d)) +
    labs(title = "Buffon's needle problem \nSimulation",
         x = "number of simulations",
         y = "prob") +
    ylim(0, 1) +
    theme_minimal()

plots <- list(p.1)

plot.save(title = title,
          plots = plots)
