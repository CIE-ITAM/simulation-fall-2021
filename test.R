library(tidyverse)
library(mvtnorm)

wishart1 <- function(k, n, mean, sigma) {
    require(mvtnorm)
    W <- list(NULL)
    for (i in 1:k) {
        X <- rmvnorm(n, mean = mean, sigma = sigma)
        W[[i]] <- t(X) %*% X
    }
    return(W)
}

wishart2 <- function(k, n, mean, sigma) {
    W <- list(NULL)
    d <- length(mean)
    A <- matrix(0, nrow = d, ncol = d)
    for (i in 1:k) {
        A[lower.tri(matrix(0, nrow = d, ncol = d))] <-
            rnorm(d * (d + 1) / 2 - d)
        diag(A) <- sqrt(rchisq(d, n - (1:d) + 1))
        L <- chol(sigma)
        W[[i]] <- L %*% A %*% t(A) %*% t(L)
    }
    return(W)
}

k <- 5
n <- 5
mean <- c(0, 0, 0, 0)
sigma <- diag(4)

set.seed(1234)

test1 <- wishart1(k, n, mean, sigma)
test2 <- wishart2(k, n, mean, sigma)

test1
test2