Stabilitetrangering <- as.data.frame(rownames(rsquaredcordf))
Stabilitetrangering$R2 <- rsquaredcordf[,1]
Stabilitetrangering <- Stabilitetrangering[order(-Stabilitetrangering$R2), ]
Stabilitetrangering$Rank <- 1:nrow(Stabilitetrangering)

for (i in 1:25) {
temprank <- rddft[order(-rddft[,i]), ]
}


Pis <- rownames(rsquaredcordf)

for (j in 1:2){
for (i in 1:ncol(totalcordf)){
  lm.spgcomb <- lm(f.tillidsammen$pfv[j:102] ~ totalcordf[j:102,i])

  R2 <- summary(lm.spgcomb)$r.squared
  stabil101[,i] <- R2
  stabiltranspose <- t(stabil101)
  stabiltranspose$comb <- Pis
  as.data.frame(assign(paste0("Stabildf", j), stabiltranspose))

}
}


# Preallocate a matrix
stabil101 <- matrix(NA, nrow = 1, ncol = ncol(totalcordf))

for (j in 1:40) {
  for (i in 1:ncol(totalcordf)) {
    y <- f.tillidsammen$pfv[j:102]
    x <- totalcordf[j:102, i]

    # Only fit if lengths match
    if (length(y) == length(x)) {
      model <- lm(y ~ x)
      stabil101[j, i] <- summary(model)$r.squared
    } else {
      stabil101[j, i] <- NA
    }
  }
}
