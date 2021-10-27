source(paste(here(), "/exercises/plot-save.R", sep = ""))
title <- "02-lgc"

# Linear congruential generator
lgc <- function(m, a, c, z0) {
    z <- z0
    i <- 1
    repeat {
        i <- i + 1
        z[i] <- (a * z[i - 1] + c) %% m
        if (i > m)
            break
    }
    return(z / m)
}

z.1 <- lgc(16, 5, 3, 7)
z.2 <- lgc(128, 7, 3, 5)
z.3 <- lgc(2048, 65, 1, 3)

df.1 <-
    data.frame(x = z.1[2:length(z.1)], y = z.1[1:length(z.1) - 1])
df.2 <-
    data.frame(x = z.2[2:length(z.2)], y = z.2[1:length(z.2) - 1])
df.3 <-
    data.frame(x = z.3[2:length(z.3)], y = z.3[1:length(z.3) - 1])

p.1 <- ggplot(df.1, aes(x = x, y = y)) +
    geom_point() +
    theme_minimal() +
    labs(
        title = "Linear congruential generator",
        x = expression(u[i]),
        y = expression(u[i - 1]),
        caption = "m = 16, a = 5, c = 3"
    )

p.2 <- ggplot(df.2, aes(x = x, y = y)) +
    geom_point() +
    theme_minimal() +
    labs(
        title = "Linear congruential generator",
        x = expression(u[i]),
        y = expression(u[i - 1]),
        caption = "m = 128, a = 7, c = 3"
    )

p.3 <- ggplot(df.3, aes(x = x, y = y)) +
    geom_point() +
    theme_minimal() +
    labs(
        title = "Linear congruential generator",
        x = expression(u[i]),
        y = expression(u[i - 1]),
        caption = "m = 2048, a = 65, c = 1"
    )

plots <- list(p.1, p.2, p.3)

plot.save(title = title,
          plots = plots)
