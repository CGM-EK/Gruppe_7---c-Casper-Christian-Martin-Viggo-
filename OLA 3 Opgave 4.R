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
rddft <- t(r2df)

####PLOT forfra####
tester <- data.frame("kombination af spørgsmål"=row.names(rddft))
for(i in 1:25){
  tester[,i+1] <- cbind(rank(-rddft[,i]))
}
colnames(tester) <- c(102:(101-24))

testerorder <- tester[order(tester$`101`),]
plotdf <- testerorder[1:5,1:25]

plotdf1 <- c(as.numeric(plotdf[1,2:25]))
plotdf2 <- c(as.numeric(plotdf[2,2:25]))
plotdf3 <- c(as.numeric(plotdf[3,2:25]))
plotdf4 <- c(as.numeric(plotdf[4,2:25]))
plotdf5 <- data.frame(as.numeric(plotdf[5,2:25]))
spgvec <- rep(plotdf$`102`, each=24)

plotdf1t <- c(plotdft[,1], plotdft[,2], plotdft[,3], plotdft[,4], plotdft[,5])
plotdffinal <- data.frame(value=as.numeric(plotdf1t), spg=spgvec)

kurt <- plotdffinal %>%
  mutate(spg = factor(spg, levels = unique(spg)))

kurt$tid <- c(rep(1:24, times=5))
ggplot(kurt, aes(x = tid, y = value, group = spg, colour=spg)) +
  geom_line(size=1.2) +
  scale_colour_brewer(palette = "Dark2")+
  scale_y_reverse()+
  geom_point() +
  theme_minimal() +
  labs(x = "Antal kvartaler fjernet (forfra)", y = "Rangering", title = "1. pladsen holder sin rangeringen stabilt igennem testen")
####PLOT forfra######

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
