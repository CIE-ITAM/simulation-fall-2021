library(tidyverse)

plot.save <- function(title, plots) {
    for (i in 1:length(plots)) {
        ggsave(
            path = paste(here(), "/exercises", sep = ""),
            filename = paste(title, "-", i, ".png", sep = ""),
            plot = plots[[i]],
            width = 15,
            height = 10,
            bg = 'white'
        )
    }
}
