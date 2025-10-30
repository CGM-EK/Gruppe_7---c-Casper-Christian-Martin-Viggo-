#Opgave 3
kvartalseq1.1 <- seq(1,307, 3)
kvartalseq2.1 <- seq(2,308, 3)
kvartalseq3.1 <- seq(3,309, 3)

kvartalerft1.1 <- f.tillid[c(kvartalseq1.1),2]
kvartalerft2.1 <- f.tillid[c(kvartalseq2.1),2]
kvartalerft3.1 <- f.tillid[c(kvartalseq3.1),2]

tempf.tillid <- as.data.frame(c((kvartalerft1.1+kvartalerft2.1+kvartalerft3.1)/3))

f.tillidsammenk3 <- as.data.frame(c(f.tillidsammen$pfv, pfvforudsigelsek3))
f.tillidsammenk3$YN <- as.factor(ifelse(f.tillidsammenk3[,1] >=0, "1", "0"))
f.tillidsammenk3$FTI <- tempf.tillid
f.tillidsammenk3$YNFTI <- as.factor(ifelse(f.tillidsammenk3[,3] >=0, "1", "0"))

f.tillidsammenk3$fittedvalues <- fitted.values(lm.opt)

glm.f.tillidsammenk3 <- glm(formula = f.tillidsammenk3$YN~f.tillidsammenk3$YNFTI, family = "binomial")
summary(glm.f.tillidsammenk3)
glmtemp <- c(as.numeric(glm.f.tillidsammenk3$fitted.values), 0)

estimateglm <- 0.1001
stdglm <- 0.3166
predictionglm <- estimateglm+stdglm*f.tillid[310,2]
#DST's indikator forudser et fald i privatforbruget i forhold til forrige år

p <- exp(predictionglm)/(1+exp(predictionglm))
#vores validering er megeeet god
