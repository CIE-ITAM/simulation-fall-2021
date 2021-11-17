library(here)
library(datasauRus)
source(paste0(here(), "/exercises/plot-save.R"))
title <- "12-datasaurus"

p <- ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset)) +
    geom_point(size = 0.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    facet_wrap(~ dataset, nrow = 3) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
    ) +
    labs(
        title = "Datasaurus",
        subtitle = paste0(
            "In this data set,\n",
            "all pairs of points have the same correlation."
        ),
        x = NULL,
        y = NULL
    )

plots <- list(p)

plot.save(title = title,
          plots = plots)
