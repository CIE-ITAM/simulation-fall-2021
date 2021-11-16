library(here)
source(paste0(here(), "/exercises/plot-save.R"))
title <- "09-multinomial-sampling"
set.seed(1234)

m <- 1000
n <- 10
p <- c(0.2, 0.1, 0.3, 0.4)

my_search <- function(x, cats) {
    z <- NULL
    for (i in 1:cats)
        z[i] <- length(which(x == i)) # observations count
    z
}

rmultinomial <- function(m, n, p) {
    r <- cumsum(p)
    X <- matrix(0, nrow = m, ncol = length(p))
    for (i in 1:m) {
        u <- runif(n)
        celdas <-
            findInterval(u, r) + 1 # compares each u with r
        X[i, ] <- my_search(celdas, length(p)) # category count
    }
    X
}

x <- rmultinomial(m, n, p)

x <- data.frame(x)

sample_hist <- function(i) {
    ggplot(x) +
        geom_histogram(
            aes_string(x = i,
                       y = "..density.."),
            binwidth = 0.25,
            fill = "#1e40ca",
            alpha = 0.8
        ) +
        labs(title = "Marginal distribution",
             x = "x") +
        theme_minimal()
}

plots <- lapply(colnames(x),
                sample_hist)

plot.save(title = title,
          plots = plots)
