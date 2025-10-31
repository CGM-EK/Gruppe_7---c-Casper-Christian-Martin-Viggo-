y <- f.tillidsammen$pfv
x_df <- as.list(totalcordf)

r2_results <- list()

for (name in names(x_df)) {
  x <- x_df[[name]]
  r2_values <- numeric(25)

  for (i in 1:25) {
    lmtemp <- lm(y[-i] ~ x[-i])
    r2_values[i] <- summary(lmtemp)$r.squared
  }

  r2_results[[name]] <- r2_values
}

r2df <- data.frame(r2_results)
rankeddf <- data.frame(rank(rddft[,2]))
rddft <- t(r2df)

tester <- as.data.frame(r2_results)
Mergetest <- merge(Stabilitetrangering,rankeddf)
#############
r2_resultsb <- list()

for (name in names(x_df)) {
  x <- x_df[[name]]
  r2_values <- numeric(25)

  for (i in 1:25) {
    lmtemp <- lm(y[i:102] ~ x[i:102])
    r2_values[i] <- summary(lmtemp)$r.squared
  }

  r2_resultsb[[name]] <- r2_values
}

r2bdf <- data.frame(r2_resultsb)

r2bdft <- t(r2bdf)
