library(tidyverse)

setwd("/Users/Wendy/github/smoke_lc/data")
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







colnames(avg) <- "fire"
p.log <-  as.data.frame(log(1455))
colnames(p.log) <- "log.fire"

## Predict
predict(model.ordernorm, newdata = p)
## Output = 6.081766 
PM10.predict <- 6.081766^10
PM10.predict

model.on <- lm(BN_PM10 ~ BN_fire, data = smoke.transform) # Adjusted R-squared:  0.7182
summary(model.on)
predict(model.on, newdata = p)
overdispersion(model.on, smoke)
require(DHARMa)
fittedModel=model.on
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)

x <-  as.data.frame(600)
colnames(x) <- "fire"
predict(model.raw, newdata = x) ## Output = 354.6858 


model.raw <- lm(PM10 ~ fire, data = smoke) # Adjusted R-squared:  0.776
summary(model.raw)

predict(model.raw, newdata = p) ## Output = 819.6423 
predict(model.raw, newdata = q) ## Output = 325.3201 
predict(model.raw, newdata = r) ## Output = 1453.723
predict(model.raw, newdata = s) ## Output = 338.9154
predict(model.raw, newdata = t) ## Output = 1453.723


model.log <- lm(log.PM10 ~ log.fire, data = smoke.transform) # Adjusted R-squared:  0.7289
summary(model.log)
predict(model.log, newdata = p)
overdispersion(model.log, smoke.transform)
require(DHARMa)
fittedModel=model.log
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)

