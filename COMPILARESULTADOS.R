library("lubridate")       # Date 
library("nowcasting")
library("tseries")
library("dynlm")           # ts regression
library("dplyr")
library(BETS)            # extract series
library(ecoseries)
library(glmnet) 
library(alfred)
library(stringr)        # string manipulation
library(FKF)            # Kalman Filtering
library(forecast)
library(randomForest)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(tis)
library("gbm")
library("tictoc")
library("stringi")
library("readxl")
library(rnn)
library(tibbletime)
library("tidyr")
library("rsample")
library("recipes")
library("tidyverse")
library(forcats)
library(timetk)
library(keras)
library(sigmoid)


if (Sys.getenv("USERNAME") =="marcos.muniz"){
  setwd("C:\\Users\\marcos.muniz\\Documents\\pucrio\\tese") 
}else if (Sys.getenv("USERNAME") =="Marcos Muniz"){
  setwd("C:\\Docs\\ECONOMETRIAIV")
}else if (Sys.getenv("USERNAME") =="marcos.muniz"){
  setwd("C:\\Users\\marcos.muniz\\Documents\\pucrio\\tese")
}else if (Sys.getenv("USERNAME") =="user"){
  setwd("C:\\Users\\user\\Documents\\Marcos")
}

predict<-stats::predict
filter <-dplyr::filter
lag <-stats::lag
rm(list=ls())
maintain <- c("maintain", "firstdate", "finaldate")

# CARREGAR CYCLE non stationary
load(file=paste0('HSrfcomplete234cycleforecasts', '.RData'))

finaldate <- mccrack[dim(mccrack)[1]-1,1]
firstdate <- mccrack[dim(mccrack)[1]-dim(FORECASTS)[1],1]
finaldate <- "2019-01-01"
FORECASTS.ts <- ts(FORECASTS,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)
cycleforecast <- na.omit(cbind(DELTAU.ts,FORECASTS.ts))

maintain <- cbind(maintain,"cycleforecast") 
rm(list=setdiff(ls(), maintain))

# CARREGAR TREND non stationary
load(file=paste0('HSrfcomplete234tendforecasts', '.RData'))

finaldate <- mccrack[dim(mccrack)[1]-1,1]
finaldate <- "2019-01-01"
firstdate <- mccrack[dim(mccrack)[1]-dim(FORECASTS)[1],1]
FORECASTS.ts <- ts(FORECASTS,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)
trendforecast <- na.omit(cbind(DELTAU.ts,FORECASTS.ts))

maintain <- cbind(maintain,"trendforecast") 
rm(list=setdiff(ls(), maintain))

# CARREGAR previsão non stationary
load(file=paste0('HSrfcomplete234forecasts', '.RData'))

finaldate <- mccrack[dim(mccrack)[1]-1,1]
finaldate <- as.Date("2019-01-01")
firstdate <- mccrack[dim(mccrack)[1]-dim(FORECASTS)[1],1]
FORECASTS.ts <- ts(FORECASTS,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)
nonstatforecast <- na.omit(cbind(DELTAU.ts,FORECASTS.ts))

maintain <- cbind(maintain,"nonstatforecast") 
rm(list=setdiff(ls(), maintain))

# CARREGAR previsão Stationary
load(file=paste0('HSrfcomplete234forecastsstationary', '.RData'))

finaldate <- mccrack[dim(mccrack)[1]-1,1]
finaldate <- as.Date("2019-01-01")
firstdate <- mccrack[dim(mccrack)[1]-dim(FORECASTS)[1],1]
FORECASTS.ts <- ts(FORECASTS,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)
statforecast <- na.omit(cbind(DELTAU.ts,FORECASTS.ts))

maintain <- cbind(maintain,"statforecast") 
rm(list=setdiff(ls(), maintain))


################ COMPARAÇÃO DOS DADOS ###########################

nonstatforecast

bestrf <- matrix(0,4,1)
bestdl <- matrix(0,4,1)
bestar <- matrix(0,4,1)
horizonvector <- c(1,3,6,12) #para quais períodos 
models <- 11
fh<-4

bestrfstat <- matrix(0,4,1)
bestdlstat <- matrix(0,4,1)
bestarstat <- matrix(0,4,1)

for(fh in 1:4){
  
  forecasthorizon <- horizonvector[fh]
  
  MSERF <- colMeans((nonstatforecast[,(14+(fh-1)*models):(16+(fh-1)*models)]-nonstatforecast[,forecasthorizon+1])^2)
  MSEDL <- colMeans((nonstatforecast[,(17+(fh-1)*models):(21+(fh-1)*models)]-nonstatforecast[,forecasthorizon+1])^2)
  MSEAR <- colMeans((nonstatforecast[,(22+(fh-1)*models):(24+(fh-1)*models)]-nonstatforecast[,forecasthorizon+1])^2)
  
  MSERFstat <- colMeans((statforecast[,(14+(fh-1)*models):(16+(fh-1)*models)]-statforecast[,forecasthorizon+1])^2)
  MSEDLstat <- colMeans((statforecast[,(17+(fh-1)*models):(21+(fh-1)*models)]-statforecast[,forecasthorizon+1])^2)
  MSEARstat <- colMeans((statforecast[,(22+(fh-1)*models):(24+(fh-1)*models)]-statforecast[,forecasthorizon+1])^2)
  
  assign(paste0("graphdata",forecasthorizon, "horizon"),cbind(nonstatforecast[,forecasthorizon+1],
                                                              nonstatforecast[,(13+which.min(MSERF)+(fh-1)*models)],
                                                              nonstatforecast[,(16+which.min(MSEDL)+(fh-1)*models)],
                                                              nonstatforecast[,(21+which.min(MSEAR)+(fh-1)*models)]))
  assign(paste0("statdata",forecasthorizon, "horizon"),cbind(statforecast[,forecasthorizon+1],
                                                             statforecast[,(13+which.min(MSERFstat)+(fh-1)*models)],
                                                             statforecast[,(16+which.min(MSEDLstat)+(fh-1)*models)],
                                                             statforecast[,(21+which.min(MSEARstat)+(fh-1)*models)]))
  
  
  assign(paste0("MSERF",forecasthorizon,"h"),MSERF)
  assign(paste0("MSEDL",forecasthorizon,"h"),MSEDL)
  assign(paste0("MSEAR",forecasthorizon,"h"),MSEAR)
  
  assign(paste0("MSERFstat",forecasthorizon,"h"),MSERFstat)
  assign(paste0("MSEDLstat",forecasthorizon,"h"),MSEDLstat)
  assign(paste0("MSEARstat",forecasthorizon,"h"),MSEARstat)
  
  bestrf[fh] <- which.min(MSERF)
  bestdl[fh] <- which.min(MSEDL)
  bestar[fh] <- which.min(MSEAR)

  bestrfstat[fh] <- which.min(MSERFstat)
  bestdlstat[fh] <- which.min(MSEDLstat)
  bestarstat[fh] <- which.min(MSEARstat)
  }

colnames(graphdata1horizon) <- c("realizado", paste0("RF",bestrf[1]), paste0("DL",bestdl[1]),paste0("AR",bestar[1]))
colnames(graphdata3horizon) <- c("realizado", paste0("RF",bestrf[2]), paste0("DL",bestdl[2]),paste0("AR",bestar[2]))
colnames(graphdata6horizon) <- c("realizado", paste0("RF",bestrf[3]), paste0("DL",bestdl[3]),paste0("AR",bestar[3]))
colnames(graphdata12horizon) <- c("realizado", paste0("RF",bestrf[4]), paste0("DL",bestdl[4]),paste0("AR",bestar[4]))

colnames(statdata1horizon) <- c("realizado", paste0("RF",bestrfstat[1]), paste0("DL",bestdlstat[1]),paste0("AR",bestarstat[1]))
colnames(statdata3horizon) <- c("realizado", paste0("RF",bestrfstat[2]), paste0("DL",bestdlstat[2]),paste0("AR",bestarstat[2]))
colnames(statdata6horizon) <- c("realizado", paste0("RF",bestrfstat[3]), paste0("DL",bestdlstat[3]),paste0("AR",bestarstat[3]))
colnames(statdata12horizon) <- c("realizado", paste0("RF",bestrfstat[4]), paste0("DL",bestdlstat[4]),paste0("AR",bestarstat[4]))

graph1hdt <- data.frame(mês=seq(firstdate,finaldate, by = "month"),
                        REAL=graphdata1horizon[,1],
                        RF=graphdata1horizon[,2],
                        DL=graphdata1horizon[,3],
                        AR=graphdata1horizon[,4])

graph1h <- ggplot(data=graph1hdt, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")+
  geom_line(linetype=1,aes(y=AR),color="red")

graph3hdt <- data.frame(mês=seq(firstdate,finaldate, by = "month"),REAL=graphdata3horizon[,1],
                        RF=graphdata3horizon[,2],
                        DL=graphdata3horizon[,3],
                        AR=graphdata3horizon[,4])

graph3h <- ggplot(data=graph3hdt, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")+
  geom_line(linetype=1,aes(y=AR),color="red")

graph6hdt <- data.frame(mês=seq(firstdate,finaldate, by = "month"),
                        REAL=graphdata6horizon[,1],
                        RF=graphdata6horizon[,2],
                        DL=graphdata6horizon[,3],
                        AR=graphdata6horizon[,4])

graph6h <- ggplot(data=graph6hdt, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")+
  geom_line(linetype=1,aes(y=AR),color="red")

graph12hdt <- data.frame(mês=seq(firstdate,finaldate, by = "month"),
                         REAL=graphdata12horizon[,1],
                         RF=graphdata12horizon[,2],
                         DL=graphdata12horizon[,3],
                         AR=graphdata12horizon[,4])
graph12h <- ggplot(data=graph12hdt, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")+
  geom_line(linetype=1,aes(y=AR),color="red")

MSEnonlinear <- matrix(0,3,4)
colnames(MSEnonlinear) <- c("1horizon", "3horizon", "6horizon", "12horizon")
rownames(MSEnonlinear) <- c("bestRF", "bestDL", "bestAR")

MSEnonlinear[1,1] <- mean((graphdata1horizon[,2]-graphdata1horizon[,1])^2)
MSEnonlinear[2,1] <- mean((graphdata1horizon[,3]-graphdata1horizon[,1])^2)
MSEnonlinear[3,1] <- mean((graphdata1horizon[,4]-graphdata1horizon[,1])^2)

MSEnonlinear[1,2] <- mean((graphdata3horizon[,2]-graphdata3horizon[,1])^2)
MSEnonlinear[2,2] <- mean((graphdata3horizon[,3]-graphdata3horizon[,1])^2)
MSEnonlinear[3,2] <- mean((graphdata3horizon[,4]-graphdata3horizon[,1])^2)

MSEnonlinear[1,3] <- mean((graphdata6horizon[,2]-graphdata6horizon[,1])^2)
MSEnonlinear[2,3] <- mean((graphdata6horizon[,3]-graphdata6horizon[,1])^2)
MSEnonlinear[3,3] <- mean((graphdata6horizon[,4]-graphdata6horizon[,1])^2)

MSEnonlinear[1,4] <- mean((graphdata12horizon[,2]-graphdata12horizon[,1])^2)
MSEnonlinear[2,4] <- mean((graphdata12horizon[,3]-graphdata12horizon[,1])^2)
MSEnonlinear[3,4] <- mean((graphdata12horizon[,4]-graphdata12horizon[,1])^2)

######################################################################################################

onehorizon <- c(14:24)
threehorizon <- c(25:35)
sixhorizon <- c(36:46)
twelvehorizon <- c(47:57)
composto1h <- matrix(0,dim(graphdata12horizon)[1],(models^2))
composto3h <- matrix(0,dim(graphdata12horizon)[1],(models^2))
composto6h <- matrix(0,dim(graphdata12horizon)[1],(models^2))
composto12h <- matrix(0,dim(graphdata12horizon)[1],(models^2))


cycle1h <- colMeans((cycleforecast[,onehorizon]-graphdata1horizon[,1])^2)
cycle3h <- colMeans((cycleforecast[,threehorizon]-graphdata3horizon[,1])^2)
cycle6h <- colMeans((cycleforecast[,sixhorizon]-graphdata6horizon[,1])^2)
cycle12h <- colMeans((cycleforecast[,twelvehorizon]-graphdata12horizon[,1])^2)

trend1h <- colMeans((trendforecast[,onehorizon]-graphdata1horizon[,1])^2)
trend3h <- colMeans((trendforecast[,threehorizon]-graphdata3horizon[,1])^2)
trend6h <- colMeans((trendforecast[,sixhorizon]-graphdata6horizon[,1])^2)
trend12h <- colMeans((trendforecast[,twelvehorizon]-graphdata12horizon[,1])^2)




for(j in 1:models){

  for(i in 1:models){
composto1h[,(i)+models*(j-1)] <- trendforecast[,min(onehorizon)+(j-1)]+cycleforecast[,min(onehorizon)+(i-1)]
composto3h[,(i)+models*(j-1)] <- trendforecast[,min(threehorizon)+(j-1)]+cycleforecast[,min(threehorizon)+(i-1)]
composto6h[,(i)+models*(j-1)] <- trendforecast[,min(sixhorizon)+(j-1)]+cycleforecast[,min(sixhorizon)+(i-1)]
composto12h[,(i)+models*(j-1)] <- trendforecast[,min(twelvehorizon)+(j-1)]+cycleforecast[,min(twelvehorizon)+(i-1)]
  }
}

trendforecast

composto1h.ts <- ts(composto1h,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)
composto3h.ts <- ts(composto3h,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)
composto6h.ts <- ts(composto6h,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)
composto12h.ts <- ts(composto12h,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)

MSEcomp1h <- colMeans((composto1h.ts - graphdata1horizon[,1])^2)
min(MSEcomp1h)
which.min(MSEcomp1h)
MSEcomp3h <- colMeans((composto3h.ts - graphdata3horizon[,1])^2)
min(MSEcomp3h)
which.min(MSEcomp3h)
MSEcomp6h <- colMeans((composto6h.ts - graphdata6horizon[,1])^2)
min(MSEcomp6h)
which.min(MSEcomp6h)
MSEcomp12h <- colMeans((composto12h.ts - graphdata12horizon[,1])^2)
min(MSEcomp12h)
which.min(MSEcomp12h)


################ COMPARAÇÃO DOS DADOS COM DECOMPOSIÇÃO ###########################
bestcomp <- matrix(0,4,1)

  assign(paste0("graphdatacomp1horizon"),cbind(graphdata1horizon,composto1h.ts[,which.min(MSEcomp1h)]))
  assign(paste0("graphdatacomp3horizon"),cbind(graphdata3horizon,composto3h.ts[,which.min(MSEcomp3h)]))
  assign(paste0("graphdatacomp6horizon"),cbind(graphdata6horizon,composto6h.ts[,which.min(MSEcomp6h)]))
  assign(paste0("graphdatacomp12horizon"),cbind(graphdata12horizon,composto12h.ts[,which.min(MSEcomp12h)]))
  
  bestcomp[1] <- which.min(MSEcomp1h)
  bestcomp[2] <- which.min(MSEcomp3h)
  bestcomp[3] <- which.min(MSEcomp6h)
  bestcomp[4] <- which.min(MSEcomp12h)

colnames(graphdatacomp1horizon) <- c("realizado", paste0("RF",bestrf[1]), paste0("DL",bestdl[1]),paste0("AR",bestar[1]),paste0("comp",bestcomp[1]))
colnames(graphdatacomp3horizon) <- c("realizado", paste0("RF",bestrf[2]), paste0("DL",bestdl[2]),paste0("AR",bestar[2]),paste0("comp",bestcomp[2]))
colnames(graphdatacomp6horizon) <- c("realizado", paste0("RF",bestrf[3]), paste0("DL",bestdl[3]),paste0("AR",bestar[3]),paste0("comp",bestcomp[3]))
colnames(graphdatacomp12horizon) <- c("realizado", paste0("RF",bestrf[4]), paste0("DL",bestdl[4]),paste0("AR",bestar[4]),paste0("comp",bestcomp[4]))


e_rf1hr <- graphdatacomp1horizon[,2]-graphdata1horizon[,1]
e_dl1hr <- graphdatacomp1horizon[,3]-graphdata1horizon[,1]
e_ar1hr <- graphdatacomp1horizon[,4]-graphdata1horizon[,1]
e_comp1hr <- graphdatacomp1horizon[,5]-graphdata1horizon[,1]
e_rf1stat <- statdata1horizon[,2]-statdata1horizon[,1]
e_dl1stat <- statdata1horizon[,3]-statdata1horizon[,1]

e_rf3hr <- graphdatacomp3horizon[,2]-graphdata3horizon[,1]
e_dl3hr <- graphdatacomp3horizon[,3]-graphdata3horizon[,1]
e_ar3hr <- graphdatacomp3horizon[,4]-graphdata3horizon[,1]
e_comp3hr <- graphdatacomp3horizon[,5]-graphdata3horizon[,1]
e_rf3stat <- statdata3horizon[,2]-statdata3horizon[,1]
e_dl3stat <- statdata3horizon[,3]-statdata3horizon[,1]

e_rf6hr <- graphdatacomp6horizon[,2]-graphdata6horizon[,1]
e_dl6hr <- graphdatacomp6horizon[,3]-graphdata6horizon[,1]
e_ar6hr <- graphdatacomp6horizon[,4]-graphdata6horizon[,1]
e_comp6hr <- graphdatacomp6horizon[,5]-graphdata6horizon[,1]
e_rf6stat <- statdata6horizon[,2]-statdata6horizon[,1]
e_dl6stat <- statdata6horizon[,3]-statdata6horizon[,1]

e_rf12hr <- graphdatacomp12horizon[,2]-graphdata12horizon[,1]
e_dl12hr <- graphdatacomp12horizon[,3]-graphdata12horizon[,1]
e_ar12hr <- graphdatacomp12horizon[,4]-graphdata12horizon[,1]
e_comp12hr <- graphdatacomp12horizon[,5]-graphdata12horizon[,1]
e_rf12stat <- statdata12horizon[,2]-statdata12horizon[,1]
e_dl12stat <- statdata12horizon[,3]-statdata12horizon[,1]


model_dm <- dm.test(e_ar1hr, e_comp1hr, alternative = "greater", h = 1, power = 2)
nonstat1hdmtest <- model_dm$p.value
model_dm <- dm.test(e_ar3hr, e_comp3hr, alternative = "greater", h = 3, power = 2)
nonstat3hdmtest <- model_dm$p.value
model_dm <- dm.test(e_ar6hr, e_comp6hr, alternative = "greater", h = 6, power = 2)
nonstat6hdmtest <- model_dm$p.value
model_dm <- dm.test(e_dl12hr, e_comp12hr, alternative = "greater", h = 12, power = 2)
nonstat12hdmtest <- model_dm$p.value

DMcomplonghorizon <- matrix(0,5,2)
colnames(DMcomplonghorizon) <- c("6 horizon", "12 horizon")
rownames(DMcomplonghorizon) <- c("BestRF", "BestDL", "BestAR", "BestRFstat", "BestDLstat")

model_dm <- dm.test(e_comp6hr,	e_rf6hr,	 alternative = "greater",	 h = 6,	 power = 2)
DMcomplonghorizon[1,1] <- model_dm$p.value
model_dm <- dm.test(e_comp6hr,	e_dl6hr,	 alternative = "greater",	 h = 6,	 power = 2)
DMcomplonghorizon[2,1] <- model_dm$p.value
model_dm <- dm.test(e_comp6hr,	e_ar6hr,	 alternative = "greater",	 h = 6,	 power = 2)
DMcomplonghorizon[3,1] <- model_dm$p.value
model_dm <- dm.test(e_comp6hr,	e_rf6stat,	 alternative = "greater",	 h = 6,	 power = 2)
DMcomplonghorizon[4,1] <- model_dm$p.value
model_dm <- dm.test(e_dl6stat,	e_rf6stat,	 alternative = "greater",	 h = 6,	 power = 2)
DMcomplonghorizon[5,1] <- model_dm$p.value


model_dm <- dm.test(e_comp12hr,e_rf12hr, alternative = "greater", h = 12, power = 2)
DMcomplonghorizon[1,2] <- model_dm$p.value
model_dm <- dm.test(e_comp12hr,e_dl12hr, alternative = "greater", h = 12, power = 2)
DMcomplonghorizon[2,2] <- model_dm$p.value
model_dm <- dm.test(e_comp12hr,e_ar12hr, alternative = "greater", h = 12, power = 2)
DMcomplonghorizon[3,2] <- model_dm$p.value
model_dm <- dm.test(e_comp12hr,e_rf12stat, alternative = "greater", h = 12, power = 2)
DMcomplonghorizon[4,2] <- model_dm$p.value
model_dm <- dm.test(e_dl12stat,e_rf12stat, alternative = "greater", h = 12, power = 2)
DMcomplonghorizon[5,2] <- model_dm$p.value  

DMcomplonghorizon <- round(DMcomplonghorizon,3)
write_excel_csv(as.data.frame(DMcomplonghorizon), "DMlonghorizontest.csv")

################################################
graphbreak <- as.data.frame(seq(firstdate,finaldate, by = "years"))


graph1hcomp <- data.frame(mês=seq(firstdate,finaldate, by = "month"),
                          REAL=graphdatacomp1horizon[,1],
                          RF=graphdatacomp1horizon[,2],
                          DL=graphdatacomp1horizon[,3],
                          AR=graphdatacomp1horizon[,4],
                          COMP=graphdatacomp1horizon[,5])

graph1hc <- ggplot(data=graph1hcomp, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")+
  geom_line(linetype=1,aes(y=AR),color="red")+
  geom_line(linetype=1,aes(y=COMP),color="green")+
  scale_x_date(date_minor_breaks="1 month")
  
graph3hcomp <- data.frame(mês=seq(firstdate,finaldate, by = "month"),REAL=graphdatacomp3horizon[,1],
                          RF=graphdatacomp3horizon[,2],
                          DL=graphdatacomp3horizon[,3],
                          AR=graphdatacomp3horizon[,4],
                          COMP=graphdatacomp3horizon[,5])

graph3hcomp <- ggplot(data=graph3hcomp, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")+
  geom_line(linetype=1,aes(y=AR),color="red")+
  geom_line(linetype=1,aes(y=COMP),color="green")+
  scale_x_date(date_minor_breaks="3 months")

graph6hcomp <- data.frame(mês=seq(firstdate,finaldate, by = "month"),
                          REAL=graphdatacomp6horizon[,1],
                          RF=graphdatacomp6horizon[,2],
                          DL=graphdatacomp6horizon[,3],
                          AR=graphdatacomp6horizon[,4],
                          COMP=graphdatacomp6horizon[,5])

graph6hc <- ggplot(data=graph6hcomp, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")+
  geom_line(linetype=1,aes(y=AR),color="red")+
  geom_line(linetype=1,aes(y=COMP),color="green")+
  scale_x_date(date_minor_breaks="6 months")

lag <-stats::lag
nivel <- window(lag(nonstatforecast[,1],-12),start= c(year(firstdate),month(firstdate)),end = c(year(finaldate),month(finaldate)))

graph12hcomp <- data.frame(mês=seq(firstdate,finaldate, by = "month"),
                           REAL=graphdatacomp12horizon[,1],
                           RF=graphdatacomp12horizon[,2],
                           DL=graphdatacomp12horizon[,3],
                           AR=graphdatacomp12horizon[,4],
                           COMP=graphdatacomp12horizon[,5])
graph12hc <- ggplot(data=graph12hcomp, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange", size = .8)+
  geom_line(linetype=1,aes(y=AR),color="red")+
  geom_line(linetype=1,aes(y=DL),color="blue", size = .8)+
  geom_line(linetype=1,aes(y=COMP),color="green", size = .8)+
  labs(x="time",y="% of Delta U", title="Realized x Forecast (12 month horizon)", subtitle="black line real Delta U - orange best RF model\nblue best DL model - green best combined decomposition")+
  scale_x_date(date_minor_breaks="12 months")


graphdatanivel <- graphdatacomp12horizon+nivel

graph12nivel <- data.frame(mês=seq(as.Date("2001-01-01"),finaldate, by = "month"),
                           REAL=graphdatanivel[,1],
                           RF=graphdatanivel[,2],
                           DL=graphdatanivel[,3],
                           AR=graphdatanivel[,4],
                           COMP=graphdatanivel[,5])
graph12hc <- ggplot(data=graph12nivel, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange",size = .8)+
  #geom_line(linetype=1,aes(y=AR),color="red")+
  #geom_line(linetype=1,aes(y=DL),color="blue")+
  geom_line(linetype=1,aes(y=COMP),color="green",size = .8)+
  labs(x="time",y="% of Delta U", title="Realized x Forecast (12 month horizon)", subtitle="black line real Delta U - orange best RF model\nblue best DL model - green best combined decomposition")+
  scale_x_date(date_minor_breaks="12 months")


####################### MSE ########################################
MSEnonlinear <- matrix(0,8,4)
colnames(MSEnonlinear) <- c("1horizon", "3horizon", "6horizon", "12horizon")
rownames(MSEnonlinear) <- c("bestRF", "bestDL", "bestAR", "bestCOMP", "cycle", "trend", "bestRFstat", "bestDLstat")

MSEnonlinear[1,1] <- mean((graphdatacomp1horizon[,2]-graphdatacomp1horizon[,1])^2)
MSEnonlinear[2,1] <- mean((graphdatacomp1horizon[,3]-graphdatacomp1horizon[,1])^2)
MSEnonlinear[3,1] <- mean((graphdatacomp1horizon[,4]-graphdatacomp1horizon[,1])^2)
MSEnonlinear[4,1] <- mean((graphdatacomp1horizon[,5]-graphdatacomp1horizon[,1])^2)
MSEnonlinear[5,1] <- min(cycle1h)
MSEnonlinear[6,1] <- min(trend1h)
MSEnonlinear[7,1] <- mean((statdata1horizon[,2]-statdata1horizon[,1])^2)
MSEnonlinear[8,1] <- mean((statdata1horizon[,3]-statdata1horizon[,1])^2)


MSEnonlinear[1,2] <- mean((graphdatacomp3horizon[,2]-graphdatacomp3horizon[,1])^2)
MSEnonlinear[2,2] <- mean((graphdatacomp3horizon[,3]-graphdatacomp3horizon[,1])^2)
MSEnonlinear[3,2] <- mean((graphdatacomp3horizon[,4]-graphdatacomp3horizon[,1])^2)
MSEnonlinear[4,2] <- mean((graphdatacomp3horizon[,5]-graphdatacomp3horizon[,1])^2)
MSEnonlinear[5,2] <- min(cycle3h)
MSEnonlinear[6,2] <- min(trend3h)
MSEnonlinear[7,2] <- mean((statdata3horizon[,2]-statdata3horizon[,1])^2)
MSEnonlinear[8,2] <- mean((statdata3horizon[,3]-statdata3horizon[,1])^2)


MSEnonlinear[1,3] <- mean((graphdatacomp6horizon[,2]-graphdatacomp6horizon[,1])^2)
MSEnonlinear[2,3] <- mean((graphdatacomp6horizon[,3]-graphdatacomp6horizon[,1])^2)
MSEnonlinear[3,3] <- mean((graphdatacomp6horizon[,4]-graphdatacomp6horizon[,1])^2)
MSEnonlinear[4,3] <- mean((graphdatacomp6horizon[,5]-graphdatacomp6horizon[,1])^2)
MSEnonlinear[5,3] <- min(cycle6h)
MSEnonlinear[6,3] <- min(trend6h)
MSEnonlinear[7,3] <- mean((statdata6horizon[,2]-statdata6horizon[,1])^2)
MSEnonlinear[8,3] <- mean((statdata6horizon[,3]-statdata6horizon[,1])^2)


MSEnonlinear[1,4] <- mean((graphdatacomp12horizon[,2]-graphdatacomp12horizon[,1])^2)
MSEnonlinear[2,4] <- mean((graphdatacomp12horizon[,3]-graphdatacomp12horizon[,1])^2)
MSEnonlinear[3,4] <- mean((graphdatacomp12horizon[,4]-graphdatacomp12horizon[,1])^2)
MSEnonlinear[4,4] <- mean((graphdatacomp12horizon[,5]-graphdatacomp12horizon[,1])^2)
MSEnonlinear[5,4] <- min(cycle12h)
MSEnonlinear[6,4] <- min(trend12h)
MSEnonlinear[7,4] <- mean((statdata12horizon[,2]-statdata12horizon[,1])^2)
MSEnonlinear[8,4] <- mean((statdata12horizon[,3]-statdata12horizon[,1])^2)

MSEnonlinear <- round(MSEnonlinear,3)

MSEnonlinear[1,]

MSEnonlinearref <- MSEnonlinear
MSEnonlinearref[,1] <- MSEnonlinear[,1]/MSEnonlinear[1,1]
MSEnonlinearref[,2] <- MSEnonlinear[,2]/MSEnonlinear[1,2]
MSEnonlinearref[,3] <- MSEnonlinear[,3]/MSEnonlinear[1,3]
MSEnonlinearref[,4] <- MSEnonlinear[,4]/MSEnonlinear[1,4]

MSEnonlinearref <- round(MSEnonlinearref,3)
write_excel_csv(as.data.frame(MSEnonlinearref), "MSEreference.csv")
######################################################################################################

MAEnonlinear <- matrix(0,7,4)
colnames(MAEnonlinear) <- c("1horizon", "3horizon", "6horizon", "12horizon")
rownames(MAEnonlinear) <- c("bestRF", "bestDL", "bestAR", "bestCOMP", "bestRFstat", "bestDLstat", "bestARstat")

MAEnonlinear[1,1] <- mean(abs(graphdatacomp1horizon[,2]-graphdatacomp1horizon[,1]))
MAEnonlinear[2,1] <- mean(abs(graphdatacomp1horizon[,3]-graphdatacomp1horizon[,1]))
MAEnonlinear[3,1] <- mean(abs(graphdatacomp1horizon[,4]-graphdatacomp1horizon[,1]))
MAEnonlinear[4,1] <- mean(abs(graphdatacomp1horizon[,5]-graphdatacomp1horizon[,1]))
MAEnonlinear[5,1] <- mean(abs(statdata1horizon[,2]-statdata1horizon[,1]))
MAEnonlinear[6,1] <- mean(abs(statdata1horizon[,3]-statdata1horizon[,1]))
MAEnonlinear[7,1] <- mean(abs(statdata1horizon[,4]-statdata1horizon[,1]))


MAEnonlinear[1,2] <- mean(abs(graphdatacomp3horizon[,2]-graphdatacomp3horizon[,1]))
MAEnonlinear[2,2] <- mean(abs(graphdatacomp3horizon[,3]-graphdatacomp3horizon[,1]))
MAEnonlinear[3,2] <- mean(abs(graphdatacomp3horizon[,4]-graphdatacomp3horizon[,1]))
MAEnonlinear[4,2] <- mean(abs(graphdatacomp3horizon[,5]-graphdatacomp3horizon[,1]))
MAEnonlinear[5,2] <- mean(abs(statdata3horizon[,2]-statdata3horizon[,1]))
MAEnonlinear[6,2] <- mean(abs(statdata3horizon[,3]-statdata3horizon[,1]))
MAEnonlinear[7,2] <- mean(abs(statdata3horizon[,4]-statdata3horizon[,1]))


MAEnonlinear[1,3] <- mean(abs(graphdatacomp6horizon[,2]-graphdatacomp6horizon[,1]))
MAEnonlinear[2,3] <- mean(abs(graphdatacomp6horizon[,3]-graphdatacomp6horizon[,1]))
MAEnonlinear[3,3] <- mean(abs(graphdatacomp6horizon[,4]-graphdatacomp6horizon[,1]))
MAEnonlinear[4,3] <- mean(abs(graphdatacomp6horizon[,5]-graphdatacomp6horizon[,1]))
MAEnonlinear[5,3] <- mean(abs(statdata6horizon[,2]-statdata6horizon[,1]))
MAEnonlinear[6,3] <- mean(abs(statdata6horizon[,3]-statdata6horizon[,1]))
MAEnonlinear[7,3] <- mean(abs(statdata6horizon[,4]-statdata6horizon[,1]))


MAEnonlinear[1,4] <- mean(abs(graphdatacomp12horizon[,2]-graphdatacomp12horizon[,1]))
MAEnonlinear[2,4] <- mean(abs(graphdatacomp12horizon[,3]-graphdatacomp12horizon[,1]))
MAEnonlinear[3,4] <- mean(abs(graphdatacomp12horizon[,4]-graphdatacomp12horizon[,1]))
MAEnonlinear[4,4] <- mean(abs(graphdatacomp12horizon[,5]-graphdatacomp12horizon[,1]))
MAEnonlinear[5,4] <- mean(abs(statdata12horizon[,2]-statdata12horizon[,1]))
MAEnonlinear[6,4] <- mean(abs(statdata12horizon[,3]-statdata12horizon[,1]))
MAEnonlinear[7,4] <- mean(abs(statdata12horizon[,4]-statdata12horizon[,1]))

MAEnonlinear <- round(MAEnonlinear,3)

######################################################################################################