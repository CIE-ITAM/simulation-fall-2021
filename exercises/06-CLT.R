source(paste0(here(), "/exercises/plot-save.R"))
title <- "06-CLT"
set.seed(1234)

CLT <- function(n, k) {
    u <- runif(k * n) # 12 observations needed for each sample
    z <- NULL
    
    for (i in 1:n) {
        z[i] <- sum(u[k * (i - 1) + 1:k]) - 6
    }
    
    df <- data.frame(z)
    
    p.1 <- ggplot(df) +
        stat_qq(aes(sample = z)) +
        theme_minimal() +
        labs(title = "Central Limit Theorem",
             caption = paste("n =", n))
    
    p.2 <- ggplot(df, aes(x = z)) +
        geom_histogram(aes(y = ..density..),
                       bins = 20,
                       fill = "#00a2ed") +
        geom_density(colour = "#1e40ca") +
        theme_minimal() +
        labs(title = "Central Limit Theorem",
             caption = paste("n =", n))
    
    return(list(p.1, p.2))
}

p.1 <- CLT(1000, 12)
p.2 <- CLT(100000, 12) # see this is much more slower

plots <- list(p.1[[1]], p.1[[2]], p.2[[1]], p.2[[2]])

plot.save(title = title,
          plots = plots)
