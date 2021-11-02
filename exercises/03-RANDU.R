source(paste0(here(), "/exercises/plot-save.R"))
title <- "03-RANDU"
options(scipen = 9)

# RANDU
seed <- as.double(1)

RANDU <- function() {
    seed <<-
        ((2 ^ 16 + 3) * seed) %% (2 ^ 31) # changes seed globally
    seed / (2 ^ 31)
}

randu <- NULL
for (i in 1:1000)
    randu[i] <- RANDU()

df <-
    data.frame(x = randu[2:length(randu)], y = randu[1:length(randu) - 1])

p.1 <- ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    theme_minimal() +
    labs(title = "RANDU",
         x = expression(u[i]),
         y = expression(u[i - 1]))

plots <- list(p.1)

plot.save(title = title,
          plots = plots)
