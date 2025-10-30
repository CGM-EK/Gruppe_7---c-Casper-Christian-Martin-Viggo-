#loader pakker til brug i opgaven
library(tidyverse)
library(ggplot2)
library(dkstat)
library(pls)
#vi henter data fra Danmarks statistik
forbrugerforv <- dst_meta(table = "FORV1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
forbrugerforv_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)
f.tillid1 <- dst_get_data(table = "FORV1", query = forbrugerforv_meta_filters, lang = "da")
f.tillid1 <- f.tillid1 %>% filter(TID >="2000-01-01")

#Folder dataframe ud på de enkelte spg.
f.tillid <- pivot_wider(
  data = f.tillid1,
  names_from = INDIKATOR,
  values_from = value)

#Angiver spgnr. på kolonner
colnames(f.tillid)[2:14] <- c("FTI", "Spg1", "Spg2", "Spg3", "Spg4", "Spg8", "Spg5", "Spg6", "Spg7", "Spg9", "Spg10", "Spg11", "Spg12")

#Erstatter NA-værdier med værdier fra samme periode året før i spg. 10
f.tillid[305,12] <- f.tillid[305-4,12]
f.tillid[306,12] <- f.tillid[306-4,12]
f.tillid[307,12] <- f.tillid[307-4,12]
f.tillid[308,12] <- f.tillid[308-4,12]
f.tillid[309,12] <- f.tillid[309-4,12]
f.tillid[310,12] <- f.tillid[310-4,12]

##########Sætter kolonne 8,9,10 og 12 i negativ - Grundet negative værdier anses som positiv for privatforbruget
f.tillid[,c(8,9,10,12)] <- f.tillid[,c(8,9,10,12)]*-1

#Vi henter data for privatforbruget fra Danmarks statistik
p.forbrugss <- dst_meta(table = "NKN1", lang = "da")

#Vi udvælger variabler vi vil kigge på og opretter et dataset
pforbrug_meta_filters <- list(
  TRANSAKT = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)
p.forbrug1 <- dst_get_data(table = "NKN1", query = pforbrug_meta_filters, lang = "da")

#Vi filtrere dataframe på tid
p.forbrug <- p.forbrug1 %>% filter(TID >="1999-01-01")

#Forbrugertillidens data sættes i kvartaler for at gøre den sammenlignelig med privatforbruget
#kvartalersekvenser opsættes for at sætte forbrugertillid dataframen op
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#Kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerft1 <- f.tillid[c(kvartalseq1),3:ncol(f.tillid)]
kvartalerft2 <- f.tillid[c(kvartalseq2),3:ncol(f.tillid)]
kvartalerft3 <- f.tillid[c(kvartalseq3),3:ncol(f.tillid)]
forbrugertillid <- as.data.frame(c((kvartalerft1+kvartalerft2+kvartalerft3)/3))

#Der oprettes en vektorer for kvartalerne fra k1 2000 til k2 2025
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-06-30"),
                 by = "quarter")
f.tillidsammen <- as.data.frame(year)

#Der oprettes en vektor for den årlige kvartalvise realvækst for privatforbruget, som indsættes i dataframes
P.forbrugvaekst <- c(0, diff(log(p.forbrug$value),lag=4)*100)
f.tillidsammen$pfv <- P.forbrugvaekst[-1]

#Kolonnenavne defineres
cols <- colnames(forbrugertillid)

#For loop der laver dataframes for hver combination af spg. i forbrugertillidsindikatoren for combinationer af 2-12
for (i in 1:12) {
  # generate all combinations of columns of size i
  Comblist <- combn(cols, i, simplify = FALSE)

  # compute correlations for each combination
  cordf <- lapply(Comblist, function(vars) {
    combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  })

  # convert to a data frame with names
  cordf_df <- as.data.frame(cordf)

  #tilføjer navne til cordf
  colnames(cordf_df)[1:ncol(cordf_df)] = sapply(Comblist, paste, collapse = " + ")

  # dynamically assign it as cordf2, cordf3, etc.
  assign(paste0("cordf", i), cordf_df)
}

#Binder alle dataframes fra for loopet i en dataframe
totalcordf <- cbind(cordf1,cordf2, cordf3, cordf4, cordf5, cordf6, cordf7, cordf8, cordf9, cordf10, cordf11, cordf12)

#Tom dataframe oprettes
tempdf = as.data.frame(matrix(data=NA,  nrow = 2, ncol = ncol(totalcordf)))

#For loop der beregner R2 og Cor og indsætter værdierne i en midlertidig dataframe
for (i in 1:ncol(totalcordf)){
  lm.spgcomb <- lm(f.tillidsammen$pfv ~ totalcordf[,i])
  fit.temp <- as.data.frame(fitted.values(lm.spgcomb))
  Cor1 <- cor(f.tillidsammen$pfv, fit.temp)
  R2 <- summary(lm.spgcomb)$r.squared
  tempdf[1,i] <- R2
  tempdf[2,i] <- Cor1
}

#Tager kolonne navne fra den midlertidige dataframe og indsætter dem i totalcordf
colnames(tempdf) <- colnames(totalcordf)

#Omnavngiver dataframe og vender den om
rsquaredcordf <- t(tempdf)

#Navngiver kolonnenavne
colnames(rsquaredcordf) <- c("R2", "COR")

#Opretter dataframe til top 5 bedste combinationer
Top5combinationer <- rsquaredcordf[order(-rsquaredcordf[,2]), ]
Top5combinationer <- as.data.frame(Top5combinationer[1:5,1:2])

#lm på privatforbrug med den bedste indikator
lm.opt <- lm(f.tillidsammen$pfv ~ totalcordf$'Spg3 + Spg8 + Spg6 + Spg9 + Spg10 + Spg11 + Spg12')
summary(lm.opt)

#definerer koefficienter for den lm
std <- 0.55351
estimate <- -4.48909
k2 <- totalcordf$'Spg3 + Spg8 + Spg6 + Spg9 + Spg10 + Spg11 + Spg12'[102]
#forudsigelse for k3
pfvforudsigelsek3 <- estimate+std*k2

spytterpåsitbarn <- (f.tillid$Spg3[310]+
                       f.tillid$Spg6[310]+
                       f.tillid$Spg8[310]+
                       f.tillid$Spg9[310]+
                       f.tillid$Spg10[310]+
                       f.tillid$Spg11[310]+
                       f.tillid$Spg12[310])/7

#forudsigelse for k4
pfvforudsigelsek4 <- estimate+std*spytterpåsitbarn

# Opgave 1.5
#Definerer Mikrospg.
Mikrospg <- as.data.frame(forbrugertillid[c(1,2,9,11,12)])
#Navngiver kolonner
Mikrocols <- colnames(Mikrospg)

#For loop der laver dataframes for hver combination af spg. i forbrugertillidsindikatoren for combinationer af 2-12
for (i in 1:5) {
  # generate all combinations of columns of size i
  Comblist <- combn(Mikrocols, i, simplify = FALSE)

  # compute correlations for each combination
  cordf <- lapply(Comblist, function(vars) {
    combo_mean <- rowMeans(Mikrospg[, vars, drop = FALSE])
  })

  # convert to a data frame with names
  cordf_df <- as.data.frame(cordf)

  #tilføjer navne til cordf
  colnames(cordf_df)[1:ncol(cordf_df)] = sapply(Comblist, paste, collapse = " + ")

  # dynamically assign it as cordf2, cordf3, etc.
  assign(paste0("Mikrocordf", i), cordf_df)
}

#Binder alle dataframes fra for loopet i en dataframe
totalMikrocordf <- cbind(Mikrocordf1, Mikrocordf2, Mikrocordf3, Mikrocordf4, Mikrocordf5)

#Tom dataframe oprettes
Mikrotempdf = as.data.frame(matrix(data=NA,  nrow = 2, ncol = ncol(totalMikrocordf)))

#For loop der beregner R2 og Cor og indsætter værdierne i en midlertidig dataframe
for (i in 1:ncol(totalMikrocordf)){
  lm.Mikrospgcomb <- lm(f.tillidsammen$pfv ~ totalMikrocordf[,i])
  fit.temp <- as.data.frame(fitted.values(lm.Mikrospgcomb))
  MikroCor1 <- cor(f.tillidsammen$pfv, fit.temp)
  MikroR2 <- summary(lm.Mikrospgcomb)$r.squared
  Mikrotempdf[1,i] <- MikroR2
  Mikrotempdf[2,i] <- MikroCor1
}

#Tager kolonne navne fra den midlertidige dataframe og indsætter dem i totalcordf
colnames(Mikrotempdf) <- colnames(totalMikrocordf)

#Omnavngiver dataframe og vender den om
Mikrorsquaredcordf <- t(Mikrotempdf)

#Navngiver kolonnenavne
colnames(Mikrorsquaredcordf) <- c("R2", "COR")

#Opretter dataframe til top 5 bedste combinationer
Top5Mikrocombinationer <- Mikrorsquaredcordf[order(-Mikrorsquaredcordf[,2]), ]
Top5Mikrocombinationer <- as.data.frame(Top5Mikrocombinationer[1:5,1:2])
