library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)
anholtWindspeed <- GET(url="https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?parameterId=wind_speed&datetime=2023-10-19T00:00:00Z/2023-10-22T00:00:00Z&stationId=06079&api-key=d5a4ceb4-12b0-4c59-8454-f5480b852a08")
anholtWinddir <- GET(url="https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?parameterId=wind_dir&datetime=2023-10-19T00:00:00Z/2023-10-22T00:00:00Z&stationId=06079&api-key=d5a4ceb4-12b0-4c59-8454-f5480b852a08")
aarhusWindspeed <- GET(url="https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?parameterId=wind_speed&datetime=2023-10-19T00:00:00Z/2023-10-22T00:00:00Z&stationId=06074&api-key=d5a4ceb4-12b0-4c59-8454-f5480b852a08")
aarhusWinddir <- GET(url="https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?parameterId=wind_dir&datetime=2023-10-19T00:00:00Z/2023-10-22T00:00:00Z&stationId=06074&api-key=d5a4ceb4-12b0-4c59-8454-f5480b852a08")

rawcontent <- content(anholtWindspeed, as="text")
rawcontentdirectionaarhus <- content(aarhusWinddir, as="text")
rawcontentdirectionanholt <- content(anholtWinddir, as="text")
rawcontentaarhus <- content(aarhusWindspeed, as="text")
data <- fromJSON(rawcontent)
dataaarhus <- fromJSON(rawcontentaarhus)
datawinddiraarhus <- fromJSON(rawcontentdirectionaarhus)
datawinddiranhotl <- fromJSON(rawcontentdirectionanholt)

dfårhusdirection <- data.frame(datawinddiraarhus[["features"]][["properties"]])
dfårhusdirection$tid <- tidspunkter
dfanholtdirection <- data.frame(datawinddiranhotl[["features"]][["properties"]])
dfanholtdirection$tid <- tidspunkter

totaldirdf <- rbind(dfanholtdirection, dfårhusdirection)
totaldirdf$stationnavn <- bynavn

tidspunkter <- seq(from = as.POSIXct("2023-10-22 00:00:00"),
                   by = "-10 min",
                   length.out = 433)

bynavn <- c(rep("Anholt", times = 433),rep("Århus", times = 433))

dfanholt <- data.frame(data[["features"]][["properties"]])
dfanholt$tid <- tidspunkter
dfaarhus <- data.frame(dataaarhus[["features"]][["properties"]])
dfaarhus$tid <- tidspunkter

totaldf <- rbind(dfanholt, dfaarhus)
totaldf$stationnavn <- bynavn
totaldf$winddir <- totaldirdf$value

#grad som viser vindhastighed
ggplot(data = totaldf, aes(y = value, x = tid, color = stationnavn))+
  geom_line()+
  theme_minimal()+
  labs(title = "Vindhastighed Anholt og Århus 2023")+ ylab("Vind M/S")+ xlab("Dato - 2023")

#graf som viser vindhastighed og vindretning
ggplot(data = totaldf, aes(x = tid, y = value, color = winddir)) +
  geom_line(size = 1) +
  facet_wrap(~stationnavn, ncol = 1) +
  scale_color_viridis_c(
    option = "turbo",
    name = "Vindretning (°)"
  ) +
  theme_minimal() +
  labs(
    title = "Vindhastighed og -retning Anholt og Århus 2023",
    x = "Dato - 2023",
    y = "Vind (m/s)"
  )

