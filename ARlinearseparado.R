if (Sys.getenv("USERNAME") =="marcos.muniz"){
  setwd("C:\\Users\\marcos.muniz\\Documents\\pucrio\\tese") 
}else if (Sys.getenv("USERNAME") =="Marcos Muniz"){
  setwd("C:\\Docs\\ECONOMETRIAIV")
}else if (Sys.getenv("USERNAME") =="marcos.muniz"){
  setwd("C:\\Users\\marcos.muniz\\Documents\\pucrio\\tese")
}else if (Sys.getenv("USERNAME") =="user"){
  setwd("C:\\Users\\user\\Documents\\Marcos")
}

rm(list=ls())

library("tidyverse")
library("lubridate")
library("readxl")
library("Metrics")


lag<-stats::lag

# Puxa dados de desemprego
unemployment <- cbind(Data = as_date(t(read_excel("Unemployment_bls.xlsx", sheet = "Base")[,1])),read_excel("Unemployment_bls.xlsx", sheet = "Base")[,2])

#transorma em time series e cria série de 1a diferença de desemprego
unemployment.ts <- ts(unemployment[,-1],start=c(year(unemployment[1,1]),month(unemployment[1,1])),end=c(year(unemployment[dim(unemployment)[1],1]),month(unemployment[dim(unemployment)[1],1])),frequency=12)
delta12u.ts <- unemployment.ts-lag(unemployment.ts,-12)

#restringe análise para 1960-2019
unemployment.ts <- window(unemployment.ts, start = c(1960,1), end = c(2019,1))
delta12u.ts <- window(delta12u.ts, start = c(1960,1), end = c(2019,1))

horizon <- 12

forecastingdates <- as.period(interval("2001-01-01","2019-01-01"))
nforecast <- forecastingdates@year*12 + forecastingdates@month + 1
firstdate <- as.Date("1960-01-01")
lastdate <- as.Date("2000-01-01")

ARForecastsU <- matrix(0,nforecast,6)
ARForecastsdel12U <- matrix(0,nforecast,6)

i<-1
for(i in 1:nforecast){

#### inicio e fim do rolling window
datainicio <- firstdate-months(1)+months(i)
datafim    <- lastdate-months(1)+months(i)

#transformar em vetor
inicio <- c(year(datainicio),month(datainicio))
fim <- c(year(datafim),month(datafim))

# Soma 6 (para absoluto) e 18 (para DEL12-sendo 6 dos AR e 12 do delta.)
inicioy <- c(year(datainicio+months(horizon+6)),month(datainicio+months(horizon+6)))
iniciodely <- c(year(datainicio+months(horizon+18)),month(datainicio+months(horizon+18)))

######## Montando o X laggado com o U absoluto
Xzao <- cbind(lag(unemployment.ts,-(horizon+0)),
              lag(unemployment.ts,-(horizon+1)),
              lag(unemployment.ts,-(horizon+2)),
              lag(unemployment.ts,-(horizon+3)),
              lag(unemployment.ts,-(horizon+4)),
              lag(unemployment.ts,-(horizon+5)))
colnames(Xzao) <- c("X1","X2","X3","X4","X5","X6")
Xzao <- window(Xzao,start=inicioy,end=fim)

X1.ts <- Xzao[,1]
X2.ts <- Xzao[,1:2]
X3.ts <- Xzao[,1:3]
X4.ts <- Xzao[,1:4]
X5.ts <- Xzao[,1:5]
X6.ts <- Xzao[,1:6]

######## Montando o X com o DELU 12
DELXzao <- cbind(lag(delta12u.ts,-(horizon+0)),
                 lag(delta12u.ts,-(horizon+1)),
                 lag(delta12u.ts,-(horizon+2)),
                 lag(delta12u.ts,-(horizon+3)),
                 lag(delta12u.ts,-(horizon+4)),
                 lag(delta12u.ts,-(horizon+5)))
colnames(DELXzao) <- c("DELX1","DELX2","DELX3","DELX4","DELX5","DELX6")
DELXzao <- window(DELXzao,,start=iniciodely,end=fim)

#tira a última observação pra ser usada no forecast
DELX1.ts <- DELXzao[-dim(DELXzao)[1],1]
DELX2.ts <- DELXzao[-dim(DELXzao)[1],1:2]
DELX3.ts <- DELXzao[-dim(DELXzao)[1],1:3]
DELX4.ts <- DELXzao[-dim(DELXzao)[1],1:4]
DELX5.ts <- DELXzao[-dim(DELXzao)[1],1:5]
DELX6.ts <- DELXzao[-dim(DELXzao)[1],1:6]

#Só considera a última observação pra ser usada no forecast
DELX1 <- DELXzao[dim(DELXzao)[1],1]
DELX2 <- DELXzao[dim(DELXzao)[1],1:2]
DELX3 <- DELXzao[dim(DELXzao)[1],1:3]
DELX4 <- DELXzao[dim(DELXzao)[1],1:4]
DELX5 <- DELXzao[dim(DELXzao)[1],1:5]
DELX6 <- DELXzao[dim(DELXzao)[1],1:6]


Y.ts <- window(unemployment.ts,start=inicioy,end=fim)
DELY.ts <- window(delta12u.ts,start=iniciodely,end=fim)

#tira a última observação pra estimação
DELY <- DELY.ts[-dim(as.matrix(DELY.ts))[1]]

#faz a estimação
DELAR1 <- lm(DELY ~ DELX1.ts)
DELAR2 <- lm(DELY ~ DELX2.ts)
DELAR3 <- lm(DELY ~ DELX3.ts)
DELAR4 <- lm(DELY ~ DELX4.ts)
DELAR5 <- lm(DELY ~ DELX5.ts)
DELAR6 <- lm(DELY ~ DELX6.ts)

ARForecastsU[i,1] <- c(1,DELX1)%*%coef(DELAR1)
ARForecastsU[i,2] <- c(1,DELX2)%*%coef(DELAR2)
ARForecastsU[i,3] <- c(1,DELX3)%*%coef(DELAR3)
ARForecastsU[i,4] <- c(1,DELX4)%*%coef(DELAR4)
ARForecastsU[i,5] <- c(1,DELX5)%*%coef(DELAR5)
ARForecastsU[i,6] <- c(1,DELX6)%*%coef(DELAR6)


}


ARForecastsU.ts <- ts(ARForecastsU,start = c(2001,1),frequency=12)
ARForecastsU.ts <- round(ARForecastsU.ts,3)
deltaurealizado <- window(delta12u.ts,start = c(2001,1))

dim(as.matrix(deltaurealizado))
dim(as.matrix(ARForecastsU.ts))


graphdata <- data.frame(mês=seq(as.Date("2001-01-01"),as.Date("2019-01-01"), by = "month"),
                        REAL=deltaurealizado,
                        AR1=ARForecastsU[,1],
                        AR2=ARForecastsU[,2],
                        AR3=ARForecastsU[,3],
                        AR4=ARForecastsU[,4],
                        AR5=ARForecastsU[,5],
                        AR6=ARForecastsU[,6])

graph <- ggplot(data=graphdata, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=AR1),color="orange")+
  geom_line(linetype=1,aes(y=AR2),color="blue")+
  geom_line(linetype=1,aes(y=AR3),color="red")+
  geom_line(linetype=1,aes(y=AR4),color="purple")+
  geom_line(linetype=1,aes(y=AR5),color="yellow")+
  geom_line(linetype=1,aes(y=AR6),color="brown")

ARForecastsU <- round(ARForecastsU,3)

mean((deltaurealizado- ARForecastsU[,1])^2)
rmse(deltaurealizado, ARForecastsU[,2])
rmse(deltaurealizado, ARForecastsU[,3])
rmse(deltaurealizado, ARForecastsU[,4])
rmse(deltaurealizado, ARForecastsU[,5])
rmse(deltaurealizado, ARForecastsU[,6])
