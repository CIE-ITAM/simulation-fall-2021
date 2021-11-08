library(tidyverse)
library(viridis)
library(MASS)

set.seed(1234)

sample <- c(1.2, 2.4, 1.3, 1.3, 0, 1, 1.8, 0.8, 4.6, 1.4)
sample.mean <- mean(sample)
sample.sd <- sd(sample)
n <- length(sample)

# a posteriori

posterior <- function(f, a, b, c, d, rate.1, rate.2) {
    aa <- seq(a, b, rate.1)
    bb <- seq(c, d, rate.2)
    
    post <- outer(aa, bb, f)
    rownames(post) = aa
    colnames(post) = bb
    
    post <- as.data.frame(post) %>%
        rownames_to_column(var = 'row') %>%
        gather(col, value,-row) %>%
        mutate(row = as.numeric(row),
               col = as.numeric(col))
    
    post <- post[!is.infinite(rowSums(post)), ]
    
    post <- na.omit(post)
    
    p <- ggplot(post, aes(
        x = row,
        y = col,
        z = value,
        fill = value
    )) +
        geom_tile() +
        geom_contour(color = 'black', size = 0.5) +
        scale_fill_viridis(option = 'mako',
                           direction = -1) +
        theme_minimal() +
        labs(x = expression(mu),
             y = expression(sigma),
             fill = NULL)
    
    p.mu <- ggplot(post, aes(x = row,
                             y = value)) +
        geom_point(size = 0.1) +
        theme_minimal() +
        labs(x = expression(mu),
             y = NULL) +
        theme(axis.text.y = element_blank())
    
    p.sigma <- ggplot(post, aes(x = col,
                                y = value)) +
        geom_point(size = 0.1) +
        theme_minimal() +
        labs(x = expression(sigma),
             y = NULL) +
        theme(axis.text.y = element_blank())
    
    return(list(p, p.mu, p.sigma, post))
}

bayes.norm <- function(a, b) {
    b ^ (-n - 1) *
        exp(-1 / (2 * b ^ 2) *
                (((n - 1) * (sample.sd ^ 2)) +
                     (n * ((sample.mean - a) ^ 2
                     ))))
}

bayes.t.1 <- function(a, b) {
    (sqrt(n) / (b * sample.sd)) *
        (1 + ((((n - 1) * (sample.sd ^ 2)
        ) +
            (n * ((sample.mean - a) ^ 2
            ))) /
            (sample.sd ^ 2))) ^ (-1)
}

bayes.t.3 <- function(a, b) {
    (sqrt(n) / (b * sample.sd)) *
        (1 + (((((n - 1) * (sample.sd ^ 2)
        ) +
            (
                n * ((sample.mean - a) ^ 2)
            )) /
            (sample.sd ^ 2)) / 3)) ^ (-2)
}

plot.norm <- posterior(bayes.norm, 0, 3, 0, 3, 0.01, 0.01)
plot.t.1 <- posterior(bayes.t.1, -3, 6, 0, 1.5, 0.01, 0.1)
plot.t.3 <- posterior(bayes.t.3, -0.5, 3.5, 0, 1.5, 0.01, 0.1)

# plot.norm[[1]]
# plot.norm[[2]]
# plot.norm[[3]]
#
# plot.t.1[[1]]
# plot.t.1[[2]]
# plot.t.1[[3]]
#
# plot.t.3[[1]]
# plot.t.3[[2]]
# plot.t.3[[3]]
