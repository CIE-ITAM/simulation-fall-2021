library(here)
library(datasauRus)
source(paste0(here(), "/exercises/plot-save.R"))
title <- "12-datasaurus"

p <- ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset)) +
    geom_point(size = 0.5) +
    theme_void() +
    theme(legend.position = "none") +
    facet_wrap( ~ dataset, nrow = 3)

plots <- list(p)

plot.save(title = title,
          plots = plots)
