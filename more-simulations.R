library(fosdata)
library(tidyverse)

options(warn = -1)

theme_set(theme_minimal())
set.seed(1234)

N <- 100000 # number of simulations

# 1. sum of 2 dice = 10

dice <- replicate(N, sample(1:6, 2, replace = T))

diceSum <- colSums(dice) # sum of two dice

prob <- cumsum(ifelse(diceSum == 10, 1, 0)) / 1:N

df <- data.frame(sim = 1:N, prob = prob)

ggplot(df, aes(x = sim, y = prob)) +
    geom_line() +
    labs(title = NULL,
         x = 'Number of simulations',
         y = 'Probability', )

p.1 <- prob[length(prob)]

# 2. get 3 tails out of 7 coin tosses

coins <- replicate(N, sample(0:1, 7, replace = T))

coinsSum <- colSums(coins) # count of tails

prob <- cumsum(ifelse(coinsSum == 3, 1, 0)) / 1:N

df <- data.frame(sim = 1:N, prob = prob)

ggplot(df, aes(x = sim, y = prob)) +
    geom_line() +
    labs(title = NULL,
         x = 'Number of simulations',
         y = 'Probability', )

p.2 <- prob[length(prob)]

# 3. sum of 5 dice between 15 and 20

dice <- replicate(N, sample(1:6, 5, replace = T))

diceSum <- colSums(dice) # sum of two dice

prob <-
    cumsum(ifelse((diceSum >= 15) & (diceSum <= 20), 1, 0)) / 1:N

df <- data.frame(sim = 1:N, prob = prob)

ggplot(df, aes(x = sim, y = prob)) +
    geom_line() +
    labs(title = NULL,
         x = 'Number of simulations',
         y = 'Probability')

p.3 <- prob[length(prob)]

# 4. 1+ empty urn(s) out of 20 with 100 randomly assigned balls

balls <- 100
urns <- 20
prob <- c()

allocation <-
    do.call('cbind', replicate(N, table(rdunif(balls, urns, 1))))

identifier <-
    matrix(
        ifelse(allocation == 0, 1, 0),
        nrow = dim(allocation)[1],
        ncol = dim(allocation)[2]
    )

prob <- cumsum(ifelse(colSums(identifier) >= 1, 1, 0)) / 1:N

df <- data.frame(sim = 1:N, prob = prob)

ggplot(df, aes(x = sim, y = prob)) +
    geom_line() +
    labs(title = NULL,
         x = 'Number of simulations',
         y = 'Probability')

p.4 <- prob[length(prob)]

# 5. first player not drawing vowels from her 7 pieces out of 100 (Scrabble)

vowels <- c('A', 'E', 'I', 'O', 'U')

game <- replicate(N, sample(scrabble$piece, 7, replace = T))

identifier <-
    matrix(ifelse(game %in% vowels, 1, 0),
           nrow = dim(game)[1],
           ncol = dim(game)[2])

prob <- cumsum(ifelse(colSums(identifier) == 0, 1, 0)) / 1:N

df <- data.frame(sim = 1:N, prob = prob)

ggplot(df, aes(x = sim, y = prob)) +
    geom_line() +
    labs(title = NULL,
         x = 'Number of simulations',
         y = 'Probability', )

p.5 <- prob[length(prob)]

# 6. P(k = 3), 1000 = c0 >= c1 >= ... >= ck-1 >= ck = 1

faces <- 1000

deathroll <- function(faces) {
    die <- c(faces)
    
    while (die[length(die)] != 1) {
        die[length(die) + 1] <- sample(1:die[length(die)], 1)
    }
    
    return(length(die))
}

game <- replicate(N, deathroll(faces))

prob <- cumsum(ifelse(game == 4, 1, 0)) / 1:N

df <- data.frame(sim = 1:N, prob = prob)

ggplot(df, aes(x = sim, y = prob)) +
    geom_line() +
    labs(title = NULL,
         x = 'Number of simulations',
         y = 'Probability', )

p.6 <- prob[length(prob)]

# 7a. obtain ace and 10, standard 52-card deck

deck <-
    rep(c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'), 4)

game <- replicate(N, sort(sample(deck, 2, replace = T)))

identifier <-
    matrix(ifelse(game == c('10', 'A'), 1, 0),
           nrow = dim(game)[1],
           ncol = dim(game)[2])

prob <- cumsum(ifelse(colSums(identifier) == 2, 1, 0)) / 1:N

df <- data.frame(sim = 1:N, prob = prob)

ggplot(df, aes(x = sim, y = prob)) +
    geom_line() +
    labs(title = NULL,
         x = 'Number of simulations',
         y = 'Probability', )

p.7a <- prob[length(prob)]

# 7b. two cards sum 19, standard 52-card deck

A <- 11
J <- 10
Q <- 10
K <- 10

deck <- rep(c(A, 2:10, J, Q, K), 4)

game <- replicate(N, sum(sample(deck, 2, replace = T)))

prob <- cumsum(ifelse(game == 19, 1, 0)) / 1:N

df <- data.frame(sim = 1:N, prob = prob)

ggplot(df, aes(x = sim, y = prob)) +
    geom_line() +
    labs(title = NULL,
         x = 'Number of simulations',
         y = 'Probability', )

p.7b <- prob[length(prob)]

# 8.

N <- 10000

# 8a. distribution of the sum of two regular dice

die1 <- sample(1:6, N, replace = TRUE) # die 1
die2 <- sample(1:6, N, replace = TRUE) # die 2

diceSum <- die1 + die2

count <- rep(0, 12)

for (i in min(diceSum):max(diceSum)) {
    count[i] <- sum(diceSum == i)
}

probs <- count / N

df <- data.frame(sum = 1:12, probs = probs)

ggplot(df, aes(x = sum, y = probs)) +
    geom_bar(stat = "identity", fill = "#1e40ca", alpha = 0.8) +
    labs(title = NULL,
         x = "Sum of the dice",
         y = "Probability") +
    scale_x_continuous(breaks = seq(1, 12, 1))

# 8b. distribution of the sum of two tweaked dice

die1 <- sample(rep(5, 6), N, replace = TRUE) # die 1
die2 <- sample(c(rep(2, 3), rep(6, 3)), N, replace = TRUE) # die 2

diceSum <- die1 + die2

count <- rep(0, 12)

for (i in min(diceSum):max(diceSum)) {
    count[i] <- sum(diceSum == i)
}

probs <- count / N

df <- data.frame(sum = 1:12, probs = probs)

ggplot(df, aes(x = sum, y = probs)) +
    geom_bar(stat = "identity", fill = "#1e40ca", alpha = 0.8) +
    labs(title = NULL,
         x = "Sum of the dice",
         y = "Probability") +
    scale_x_continuous(breaks = seq(1, 12, 1))

# 8c. distribution of the sum of two tweaked dice

die1 <- sample(c(1, 2, 2, 3, 3, 4), N, replace = TRUE) # die 1
die2 <- sample(c(1, 3, 4, 5, 6, 8), N, replace = TRUE) # die 2

diceSum <- die1 + die2

count <- rep(0, 12)

for (i in min(diceSum):max(diceSum)) {
    count[i] <- sum(diceSum == i)
}

probs <- count / N

df <- data.frame(sum = 1:12, probs = probs)

ggplot(df, aes(x = sum, y = probs)) +
    geom_bar(stat = "identity", fill = "#1e40ca", alpha = 0.8) +
    labs(title = NULL,
         x = "Sum of the dice",
         y = "Probability") +
    scale_x_continuous(breaks = seq(1, 12, 1))
