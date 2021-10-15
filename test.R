library(tidyverse)

lambda.t <- function(t, n) {
  x <- paste('', '{', 0, '< t & t <=', 1, '}',
             sep = '')
  for (i in seq(2, n, 2)) {
    x <- paste(x,
               paste('', '{', i, '< t & t <=', i + 1, '}',
                     sep = ''),
               sep = '|')
  }
  return(ifelse(eval(parse(text = x)), 3, 5))
}

inhomogeneousPoisson <- function(n) {
  s <- cumsum(rexp(n, 5))
  u <- runif(n)
  points <- s[u <= lambda.t(s, n) / 5]
  count <- 1:length(points)
  df <- data.frame(points, count)
  return(df)
}

inhomogeneousPoisson(500) %>%
  ggplot(aes(x = points, y = count)) +
  geom_step() +
  labs(title = 'Simulación de un proceso Poisson no homogéneo',
       x = 'Puntos (s)',
       y = 'N(t)') +
  theme_minimal()

inhomogeneousPoisson(140) %>%
  ggplot(aes(x = points, y = count)) +
  geom_step() +
  labs(title = 'Simulación de un proceso Poisson no homogéneo',
       x = 'Puntos (s)',
       y = 'N(t)') +
  theme_minimal()

fr <- NULL
N <- 10000

for (i in 1:N) {
  x <- inhomogeneousPoisson(10)
  
  fr <- c(fr, ifelse(sum(x$points[x$count > 2] > 1.25 &
                           x$points[x$count > 2] <= 3) > 0, 1, 0))
}
sum(fr) / N
