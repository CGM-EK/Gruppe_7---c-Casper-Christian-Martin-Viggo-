#### PCR.fit ####
install.packages("pls")
library(pls)

pcr.fit <- pcr(f.tillidsammen$pfv ~ forbrugertillid$Spg1+
                 forbrugertillid$Spg2+
                 forbrugertillid$Spg3+
                 forbrugertillid$Spg4+
                 forbrugertillid$Spg5+
                 forbrugertillid$Spg6+
                 forbrugertillid$Spg7+
                 forbrugertillid$Spg8+
                 forbrugertillid$Spg9+
                 forbrugertillid$Spg10+
                 forbrugertillid$Spg11+
                 forbrugertillid$Spg12,
               validation = "CV", scale = T)
summary(pcr.fit)

loadings.pcr.fit <- pcr.fit$loadings

validationplot(pcr.fit, val.type = "MSEP")

w.indicators1 <- as.data.frame(loadings.pcr.fit[1:12, 2]^2)
sum(w.indicators1)

w.indicators1$spg <- c("Spg1", "Spg2", "Spg3", "Spg4", "Spg5", "Spg6", "Spg7", "Spg8", "Spg9", "Spg10", "Spg11", "Spg12")
w.indicators1$spg <- factor(w.indicators1$spg,
                            levels = paste0("Spg", 1:12))

ggplot(data = w.indicators1, aes(x = spg[1:12], y = w.indicators1[,1]))+
  geom_bar(fill = "darkolivegreen4", stat = "identity")+
  ylab("Vægt")+xlab("Spørgsmål")+ labs(title = "Vægtning af spørgsmål")+
  theme_minimal()

# pcr.fit på spg for den optimale indikator fra opg. 1
pcr.fit.opt <- pcr(f.tillidsammen$pfv ~
                     forbrugertillid$Spg3+
                     forbrugertillid$Spg8+
                     forbrugertillid$Spg9+
                     forbrugertillid$Spg11+
                     forbrugertillid$Spg12,
                   validation = "CV", scale = T)
summary(pcr.fit.opt)

loadings.pcr.fit.opt <- pcr.fit.opt$loadings

validationplot(pcr.fit.opt, val.type = "MSEP")

w.indicators1opt <- as.data.frame(loadings.pcr.fit.opt[1:5, 2]^2)
sum(w.indicators1opt)

w.indicators1opt$spg <- c("Spg3", "Spg8", "Spg9", "Spg11", "Spg12")
w.indicators1opt$spg <- factor(w.indicators1opt$spg,
                               levels = paste0("Spg", 1:12))

ggplot(data = w.indicators1opt, aes(x = spg[1:5], y = w.indicators1opt[,1]))+
  geom_bar(fill = "darkolivegreen4", stat = "identity")+
  ylab("Vægt")+xlab("Spørgsmål")+ labs(title = "Vægtning af spørgsmål på den optimale indikator")+
  theme_minimal()

#pfv
optimalvektor <- as.data.frame(forbrugertillid$Spg3*as.numeric(w.indicators1[3,1])+
                                 forbrugertillid$Spg8*as.numeric(w.indicators1[8,1])+
                                 forbrugertillid$Spg9*as.numeric(w.indicators1[9,1])+
                                 forbrugertillid$Spg11*as.numeric(w.indicators1[11,1])+
                                 forbrugertillid$Spg12*as.numeric(w.indicators1[12,1]))

lm.opt.vægt <- lm(f.tillidsammen$pfv ~ optimalvektor[,1])
summary(lm.opt.vægt)

k3opt <- optimalvektor[99,1]
#definerer koefficienter for den lm
stdopt <- 1.2646
estimateopt <- -8.4992

#forudsigelse for k3
pfvforudsigelsek3opt <- estimateopt+stdopt*k3opt

# Opgave 2.4
pfv2015 <- sum(p.forbrug[65:68,5])
pfv2016 <- sum(p.forbrug[69:72,5])
dif <- (pfv2016/pfv2015*100)-100
# PFV faktiskevægt fra 2015 til 2016 = 3.28%
