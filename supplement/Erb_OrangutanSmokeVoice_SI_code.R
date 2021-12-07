library(tidyverse)

setwd('/Users/Me/github/orangutan-smoke-voice/supplement')

smoke <- read.csv('PM10_FireHotspot_PKY.csv')

## Model building using fire hotspots and PM10 data for Palangkaraya
## 100_7d: R2 = 0.7987427
PM10_fire.100.7 <- lm(log(PM10) ~ log(X100km_7d.1), data = smoke)
summary(PM10_fire.100.7)

## 50_7d: R2 = 0.8104033
PM10_fire.50.7 <- lm(log(PM10) ~ log(X50km_7d.1), data = smoke)
summary(PM10_fire.50.7)

## 25_7d: R2 = 0.8098656
PM10_fire.25.7 <- lm(log(PM10) ~ log(X25km_7d.1), data = smoke)
summary(PM10_fire.25.7)

## 100_3d: R2 = 0.7850568
PM10_fire.100.3 <- lm(log(PM10) ~ log(X100km_3d.1), data = smoke)
summary(PM10_fire.100.3)

## 50_3d: R2 = 0.7901593
PM10_fire.50.3 <- lm(log(PM10) ~ log(X50km_3d.1), data = smoke)
summary(PM10_fire.50.3)

## 25_3d: R2 = 0.7676992
PM10_fire.25.3 <- lm(log(PM10) ~ log(X25km_3d.1), data = smoke)
summary(PM10_fire.25.3)

## 100_2d: R2 = 0.7663484
PM10_fire.100.2 <- lm(log(PM10) ~ log(X100km_2d.1), data = smoke)
summary(PM10_fire.100.2)

## 50_2d: R2 = 0.7621278
PM10_fire.50.2 <- lm(log(PM10) ~ log(X50km_2d.1), data = smoke)
summary(PM10_fire.50.2)

## 25_2d: R2 = 0.7256116
PM10_fire.25.2 <- lm(log(PM10) ~ log(X25km_2d.1), data = smoke)
summary(PM10_fire.25.2)

## Plot for Fig. S1.2
ggplot(smoke, aes(log(X50km_7d.1), log(PM10))) +
  geom_point() +
  geom_smooth() +
  theme_classic()+
  xlab("log(7-day fire count)")


## Use model created using PKY data to predict PM10 at Tuanan using fire hotspots surrounding study site (hotspot data retrieved from GFED)
list <- read.csv('tuanan_hotspot_50km.csv')
a <-  as.data.frame(list$LOG)
colnames(a) <- "log_50km7d"
model.raw <- lm(log_pm10 ~ log_50km7d, data = smoke)
predictlogPM10 <- predict(model.raw, newdata = a)  


## Save results of Tuanan PM10 estimated values as csv file
write.csv(predictlogPM10,'tuanan_hotspot-to-pm10_50km.csv')

