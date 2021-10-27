library(tidyverse)

plot.save <- function(title, plots) {
    for (i in 1:length(plots)) {
        ggsave(
            path = paste(here(), "/exercises", sep = ""),
            filename = paste(title, "_", i, ".svg", sep = ""),
            plot = plots[[i]],
            width = 15,
            height = 10
        )
    }
}
