#required libraries
library(tidyverse)
library(mosaic)
library(stargazer)
#df format conversion + variable naming
as.numeric(as.character(Dataex1$time))
as.numeric(as.character(Dataex1$sales))
colnames(Dataex1) <- c('date','sales')

#1. plot 1, sales given date
plot1 <- ggplot(data = Dataex1) +
  geom_point(aes(x = date, y = sales)) +
  labs(x = "Date",
       y = "Sales",
       title = "Sales given Date") +
theme_minimal()

#2. plot 1, linear regression
Dataex1$time <- 1:36
eq1 <- lm(sales ~ time, data = Dataex1)
summary(eq1)

#3. Dummy variables / name == month
Dataex1$july <- seq.int(1,12, by=1)
Dataex1$july <- ifelse(Dataex1$july==7,1,0)

Dataex1$aug <- seq.int(1,12, by=1)
Dataex1$aug <- ifelse(Dataex1$aug==8,1,0)

Dataex1$sept <- seq.int(1,12, by=1)
Dataex1$sept <- ifelse(Dataex1$sept==9,1,0)

Dataex1$oct <- seq.int(1,12, by=1)
Dataex1$oct <- ifelse(Dataex1$oct==10,1,0)

Dataex1$nov <- seq.int(1,12, by=1)
Dataex1$nov <- ifelse(Dataex1$nov==11,1,0)

Dataex1$dec <- seq.int(1,12, by=1)
Dataex1$dec <- ifelse(Dataex1$dec==12,1,0)

Dataex1$jan <- seq.int(1,12, by=1)
Dataex1$jan <- ifelse(Dataex1$jan==1,1,0)

Dataex1$feb <- seq.int(1,12, by=1)
Dataex1$feb <- ifelse(Dataex1$feb==2,1,0)

Dataex1$march <- seq.int(1,12, by=1)
Dataex1$march <- ifelse(Dataex1$march==3,1,0)

Dataex1$apr <- seq.int(1,12, by=1)
Dataex1$apr <- ifelse(Dataex1$dec==4,1,0)

Dataex1$may <- seq.int(1,12, by=1)
Dataex1$may <- ifelse(Dataex1$may==5,1,0)

Dataex1$jun <- seq.int(1,12, by=1)
Dataex1$jun <- ifelse(Dataex1$jun==6,1,0)

# 4. Regression based on  time and dummy >nope
eqJan <- lm(sales ~ time + jan, data = Dataex1)
eqFeb <- lm(sales ~ time + feb, data = Dataex1)
eqMar <- lm(sales ~ time + march, data = Dataex1)
eqApr <- lm(sales ~ time + apr, data = Dataex1)
eqMay <- lm(sales ~ time + may, data = Dataex1)
eqJun <- lm(sales ~ time + jun, data = Dataex1)
eqJul <- lm(sales ~ time + july, data = Dataex1)
eqAug <- lm(sales ~ time + aug, data = Dataex1)
eqSept <- lm(sales ~ time + sept, data = Dataex1)
eqOct <- lm(sales ~ time + oct, data = Dataex1)
eqNov <- lm(sales ~ time + nov, data = Dataex1)
eqDec <- lm(sales ~ time + dec, data = Dataex1)

#final regres
eqtot <- lm(sales ~ time + jan + feb + march + apr + may + jun + july + aug + sept + oct +nov + dec, data = Dataex1)
stargazer(eqtot,type="text",out="dummyreq.txt")

# predicting sales
Dataex1$sales[31] <- (0.045*31)+(0.167)+(2.817)
Dataex1$sales[32] <- (0.045*32)+(0.232)+(2.817)
Dataex1$sales[33] <- (0.045*33)-(0.048)+(2.817)
Dataex1$sales[34] <- (0.045*34)-(0.118)+(2.817)
Dataex1$sales[35] <- (0.045*35)-(0.538)+(2.817)
Dataex1$sales[36] <- (0.045*36)-(0.658)+(2.817)

#graph with predicted sales

plot2 <- ggplot(data = Dataex1) +
  geom_point(aes(x = date, y = sales)) +
  labs(x = "Date",
       y = "Sales",
       title = "Sales given Date with predicted sales") +
  theme_minimal()

# 5. 95% interval = roughly regression +/- 2se for Dec 1992
upperinterval <- (0.045*((Dataex1$sales))+1)-(0.658)+(2.817)+(2*0.167)
lowerinterval <- (0.045*36)-(0.658)+(2.817)-(2*0.167)

# 6. dataframe showing adjusted and unadjusted forecasts
month <- c('July','August','September','October','November','December')
forecastnoadj <-c(4.379, 4.489, 4.254, 4.229, 3.854, 3.779)
sfratio <- c(0.987494059,0.988075745,0.988748444,0.989258015,0.986018112,0.984902187)
forecastadj <- forecastnoadj*sfratio
adjdata <- data.frame(month,forecastnoadj,sfratio,forecastadj)
