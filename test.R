library(tidyverse)
library(berryFunctions)

exponential_memoryless <- function(N, lambda) {
    t <- sample(1:10, N, replace = TRUE) # t
    s <- sample(1:10, N, replace = TRUE) # s
    
    # P(X > t + s | X > t)
    prob.1 <- (1 - pexp(t + s, lambda)) / (1 - pexp(t, lambda))
    
    # P(X > s)
    prob.2 <- 1 - pexp(s, lambda)
    
    return(tibble(prob.1, prob.2, almost.equal(prob.1, prob.2)))
}

lambda <- 1 / 3 # Parámetro
r.1 <- exponential_memoryless(100, lambda) # 100 simulaciones
r.2 <- exponential_memoryless(1000000, lambda) # 1 000 000 simulaciones

r.1[r.1[,3] == FALSE,]
r.2[r.2[,3] == FALSE,]