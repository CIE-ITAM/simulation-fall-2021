library(tidyverse)

x.0 <- seq(0, 12, 1)

x.1 <- matrix(0, nrow = 13, ncol = 13)

for (i in 1:13) {
    x.1[, i] <- (x.0 / 12) + i
}

x.1 <- data.frame(x.1)

x.1 <- x.1 - 1

x.1

ggplot(x.1) +
    geom_line(aes(x = X1 * 12, y = X1)) +
    geom_line(aes(x = X1 * 12, y = X2)) +
    geom_line(aes(x = X1 * 12, y = X3)) +
    geom_line(aes(x = X1 * 12, y = X4)) +
    geom_line(aes(x = X1 * 12, y = X5)) +
    geom_line(aes(x = X1 * 12, y = X6)) +
    geom_line(aes(x = X1 * 12, y = X7)) +
    geom_line(aes(x = X1 * 12, y = X8)) +
    geom_line(aes(x = X1 * 12, y = X9)) +
    geom_line(aes(x = X1 * 12, y = X10)) +
    geom_line(aes(x = X1 * 12, y = X11)) +
    geom_line(aes(x = X1 * 12, y = X12)) +
    geom_line(aes(x = X1, y = X1 * 12)) +
    geom_line(aes(x = X2, y = X1 * 12)) +
    geom_line(aes(x = X3, y = X1 * 12)) +
    geom_line(aes(x = X4, y = X1 * 12)) +
    geom_line(aes(x = X5, y = X1 * 12)) +
    geom_line(aes(x = X6, y = X1 * 12)) +
    geom_line(aes(x = X7, y = X1 * 12)) +
    geom_line(aes(x = X8, y = X1 * 12)) +
    geom_line(aes(x = X9, y = X1 * 12)) +
    geom_line(aes(x = X10, y = X1 * 12)) +
    geom_line(aes(x = X11, y = X1 * 12)) +
    geom_line(aes(x = X12, y = X1 * 12)) +
    scale_x_continuous(breaks = seq(0, 12, 1)) +
    scale_y_continuous(breaks = seq(0, 12, 1)) +
    theme_minimal() +
    labs(x = 'minutes',
         y = 'hours')
