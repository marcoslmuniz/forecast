start_time <- Sys.time()

library("tidyverse")
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
library(tidyverse)
library(forcats)
library(timetk)
library(keras)
library(sigmoid)
library("dlm")
library("mFilter")

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
lag <- stats::lag
#puxa os dados do McCracken
mccrackenbase <- as.data.frame(read.csv("current.csv",dec=".",header = TRUE,sep = "," ))
mccrackenbase <- mccrackenbase[-c(1,dim(mccrackenbase)[1]),]
Codes <- mccrackenbase[1,]
datamc <- mdy(t(mccrackenbase[,1]))

#puxa dados de desemprego dos estados unidos
unemployment <- cbind(Data = as_date(t(read_excel("Unemployment_bls.xlsx", sheet = "Base")[,1])),read_excel("Unemployment_bls.xlsx", sheet = "Base")[,2])
lagunemploy  <- cbind(Data = as_date(t(read_excel("Unemployment_bls.xlsx", sheet = "Baselaggada")[,1])),read_excel("Unemployment_bls.xlsx", sheet = "Baselaggada")[,2:26])
DELTAU.ts <- ts(lagunemploy[,-1],start=c(year(lagunemploy[1,1]),month(lagunemploy[1,1])),end=c(year(lagunemploy[dim(lagunemploy)[1],1]),month(lagunemploy[dim(lagunemploy)[1],1])),frequency=12)
DEL1U.ts <- window(DELTAU.ts,start=c(year(datamc[1]),month(datamc[1])),end=c(year(datamc[dim(as.matrix(datamc))[1]]),month(datamc[dim(as.matrix(datamc))[1]])))

#Formata datas na primeira coluna da matriz
mccrack <- cbind(Data = as.data.frame(as_date(datamc)),mccrackenbase[,-1], as.matrix(DEL1U.ts[,2]))


# Declaração de parâmetros
beginning <- dmy("01/01/1960")
window    <- 12*40-1 #número de anos*12meses - um, equivalente à Jan/60
i<-1
test <- 12*10 #(   years)
hiddenstates <- 4
horizonvector <- c(1,3,6,12,24) #para quais períodos 


models <- 11
models1horizon <- matrix(c("1hRF1lag", "1hRF3lag", "1hRF6lag", "1hRFhs1lag", "1hRFhs3lag", "1hRFhs6lag","1hLSTMcalc", "1hlstmpred","AR1lag","AR3lag","AR6lag"),models,1)
models3horizon <- matrix(c("3hRF1lag", "3hRF3lag", "3hRF6lag", "3hRFhs1lag", "3hRFhs3lag", "3hRFhs6lag","3hLSTMcalc", "3hlstmpred","AR1lag","AR3lag","AR6lag"),models,1)
models6horizon <- matrix(c("6hRF1lag", "6hRF3lag", "6hRF6lag", "6hRFhs1lag", "6hRFhs3lag", "6hRFhs6lag","6hLSTMcalc", "6hlstmpred","AR1lag","AR3lag","AR6lag"),models,1)
models12horizon <- matrix(c("12hRF1lag", "12hRF3lag", "12hRF6lag", "12hRFhs1lag", "12hRFhs3lag", "12hRFhs6lag","12hLSTMcalc", "12hlstmpred","AR1lag","AR3lag","AR6lag"),models,1)
models24horizon <- matrix(c("24hRF1lag", "24hRF3lag", "24hRF6lag", "24hRFhs1lag", "24hRFhs3lag", "24hRFhs6lag","24hLSTMcalc", "24hlstmpred","AR1lag","AR3lag","AR6lag"),models,1)

# Criação da matriz de datas que serve de base para as rollingwindows
forecastingdates <- as.period(interval(beginning+months(window),unemployment[dim(unemployment)[1],1]))
nforecast <- forecastingdates@year*12 + forecastingdates@month
rollingwindow <- as.Date(matrix(c(beginning + months(0),beginning + months(window)),nforecast+20,ncol=2,byrow = TRUE))
rollingwindow[,1] <- rollingwindow[,1]+months(c(0:(dim(rollingwindow)[1]-1)))
rollingwindow[,2] <- rollingwindow[,2]+months(c(0:(dim(rollingwindow)[1]-1)))

#transformando em timeseries data
mccrack.ts <- ts(mccrack[,-1],start=c(year(mccrack[1,1]),month(mccrack[1,1])),end=c(year(mccrack[dim(mccrack)[1],1]),month(mccrack[dim(mccrack)[1],1])),frequency=12)
unemployment.ts <- ts(unemployment[,-1],start=c(year(unemployment[1,1]),month(unemployment[1,1])),end=c(year(unemployment[dim(unemployment)[1],1]),month(unemployment[dim(unemployment)[1],1])),frequency=12)
delu.ts <- ts(mccrack$UNRATE,start=c(year(mccrack[1,1]),month(mccrack[1,1])),end=c(year(mccrack[dim(mccrack)[1],1]),month(mccrack[dim(mccrack)[1],1])),frequency=12)

#Matrizes que serão abastecidasw com as Matrizes de Weight
Wx123e4 <- matrix(0,(dim(mccrackenbase)[2]-1),nforecast*hiddenstates*4)
Wh123e4 <- matrix(0,hiddenstates,nforecast*hiddenstates*4)
B123e4  <- matrix(0,hiddenstates*4,nforecast*1)
Wy      <- matrix(0,hiddenstates,nforecast*1)
By      <- matrix(0,1,nforecast)
htes    <- matrix(0,window,nforecast*hiddenstates)
htforecast    <- matrix(0,window,nforecast*hiddenstates)
FORECASTS <- matrix(0,nforecast,dim(as.matrix(horizonvector))[1]*models)
colnames(FORECASTS) <- cbind(models1horizon,models3horizon,models6horizon,models12horizon,models24horizon)
lags <- matrix(c(1,3,6),3,1)

tic("LSTM")
i <- 1
fh <- 5

for(fh in 1:dim(as.matrix(horizonvector))[1]){
forecasthorizon <- horizonvector[fh]
######################### INÍCIO DO "FOR" COM OS MODELOS LSTM ######################################

batch_size <- 12

for(i in 1:((nforecast-forecasthorizon))){
  
  Xrwindow <- t(na.omit(t(window(mccrack.ts, start=c(year(ymd(rollingwindow[i,1])),month(rollingwindow[i,1])),end=c(year(ymd(rollingwindow[i+1-forecasthorizon,2])),month(rollingwindow[i+1-forecasthorizon,2]))))))
  Yrwindow <- window(unemployment.ts,start=c(year(ymd(rollingwindow[i,1])),month(rollingwindow[i,1])),end=c(year(ymd(rollingwindow[i,2])),month(rollingwindow[i,2])))
  Y2window <- as.numeric(window(DELTAU.ts[,forecasthorizon+1],start=c(year(ymd(rollingwindow[i+forecasthorizon,1])),month(rollingwindow[i+forecasthorizon,1])),end=c(year(ymd(rollingwindow[i,2])),month(rollingwindow[i,2]))))
  #Zrwindow <- t(na.omit(t(window(mccrack.ts, start=c(year(ymd(rollingwindow[i+1,1])),month(rollingwindow[i+1,1])),end=c(year(ymd(rollingwindow[i+1,2])),month(rollingwindow[i+1,2]))))))
  
  ######################### Filtro HP #####################

  teste <- hpfilter(Yrwindow, type="lambda", freq = 14400)
  datas <- seq(from=as_date(rollingwindow[i,1]), to=as_date(rollingwindow[i,2]), by='month')
  df <- data.frame(abs=Yrwindow, z=as.data.frame(teste$trend),Data=datas)
  filtro <- ggplot(data=df, aes(x=Data,y=100*abs))+
   geom_line()+
   geom_line(linetype=2,aes(y=100*Series.1),color="red")
  
  Y2window <- teste$cycle-lag(teste$cycle,-forecasthorizon)
  
  ############################ FIltro HP ######################
  
  XeZ <- scale(Xrwindow)
  
  mean <- mean(Yrwindow)
  sigma <- sd(Yrwindow)
  mean2 <- mean(Y2window)
  sigma2<- sd(Y2window)
  
  X <- XeZ[-dim(XeZ)[1],]
  X <- X[(nrow(X)-(nrow(X) %/% batch_size * batch_size)+1):nrow(X),]
  Y <- scale(Yrwindow)
  Y2<- scale(Y2window)
  Y2<- as.matrix(Y2[(nrow(Y2)-(nrow(Y2) %/% batch_size * batch_size)+1):nrow(Y2),])
  Z <- XeZ[-1,]
  
  x_train <- array(X[1:(dim(X)[1]-test),],dim = c((dim(X)[1]-test),  1,dim(X)[2]))
  y_train <- array(Y[1:(dim(Y)[1]-test),],dim = c((dim(Y)[1]-test),  1,dim(Y)[2]))
  y2_train<- array(Y2[1:(dim(Y2)[1]-test)],dim = c((dim(Y2)[1]-test),  1,dim(Y2)[2]))
  z_train <- array(Z[1:(dim(Z)[1]-test),],dim = c((dim(Z)[1]-test),  1,dim(Z)[2]))
  
  if(test == 0){
    x_valid <- NULL
    y_valid <- NULL
    y2_train<- NULL
    z_valid <- NULL
  }  else {
    x_valid <- array(X[(dim(X)[1]-test+1):dim(X)[1],],dim = c(test,  1,dim(X)[2]))
    y_valid <- array(Y[(dim(Y)[1]-test+1):dim(Y)[1],],dim = c(test,  1,dim(Y)[2]))
    y2_valid <- array(Y2[(dim(Y2)[1]-test+1):dim(Y2)[1],],dim = c(test,  1,dim(Y2)[2]))
    z_valid <- array(Z[(dim(Z)[1]-test+1):dim(Z)[1],],dim = c(test,  1,dim(Z)[2]))
  }
  
  #------------------------------------ DECLARAR MODELO LSTM ------------------------------------------------#
  #print_dot_callback <- callback_lambda(
  #  on_epoch_end = function(epoch, logs) {
  #    if (epoch %% 80 == 0) cat("\n")
  #    cat(".")
  #  }
  #)
  rodar <- "yes"
  try <- 1
  for(try in 1:3){

  tries <- c(21,51,5)
        
  if(rodar == "yes"){  
  
  stopcallback <- callback_early_stopping(
    monitor = "val_mean_squared_error", 
    min_delta = 0.00,
    patience = (tries[try]-1),
    verbose = 0,
    mode = c("auto", "min", "max"),
    baseline = NULL, 
    restore_best_weights = FALSE)
  
  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(
      units = 4, 
      # the first layer in a model needs to know the shape of the input data
      # return_sequences = TRUE  #DEVERIA RETORNAR OS HIDDEN STATES 
      batch_input_shape  = c(batch_size, 1, dim(X)[2]),
      dropout = 0.1,
      recurrent_dropout = 0.1,
      # by default, an LSTM just returns the final state
      return_sequences = TRUE
    ) %>% 
    #   layer_dropout(0.4) %>%
    time_distributed(layer_dense(units = 1))
  
  model %>%
    compile(
      loss = "logcosh",
      optimizer = "Nadam", #NUNCA usar Adagrad e Adadelta (muito ruins). Para i<- 288 Nadam > Adamax > Adam
      # in addition to the loss, Keras will inform us about current 
      # MSE while training
      metrics = list("mean_squared_error")
    )
  
  
  history <- model %>% fit(
    x          = x_train,
    #Y          = y_train,
    y          = y2_train,
    #  validation_data = list(x_valid, y_valid),
    validation_data = list(x_valid, y2_valid),
    batch_size = batch_size,
    epochs     = 350,
    shuffle=FALSE,
    callbacks = list(stopcallback)
  )
  
  repeticoes <- dim(as.matrix(history$metrics$loss))[1]
  if(repeticoes == tries[try]){
    rodar <- "yes"
    
  }
  else{ rodar <- "no"}
  
  }
    
  else{
      
  }
    
  }
  
  pesos <- get_weights(model)
  
  #yhat = model %>% predict(z_valid, batch_size=batch_size)
  DELyhat = model %>% predict(z_valid, batch_size=batch_size)
  
  #FORECASTS[i,8] <- yhat[60]*desvio+mean  
  FORECASTS[i+forecasthorizon,8+(fh-1)*models] <- DELyhat[60]*sigma2+mean2  
  
  #para salvar as matrizes de peso
  Wx123e4[1:(dim(x_train)[3]),((i-1)*hiddenstates*4+1):(i*hiddenstates*4)] <- pesos[[1]]
  Wh123e4[,((i-1)*hiddenstates*4+1):(i*hiddenstates*4)] <- pesos[[2]]
  B123e4[,i] <- pesos[[3]]
  Wy[,i] <- pesos[[4]]
  By[i] <- pesos[[5]]
  
  #Separando as matrizes de pesos em notação mais palpável
  Wxi <- Wx123e4[1:(dim(x_train)[3]),((i-1)*hiddenstates*4+1):(i*hiddenstates*4-12)]
  Wxf <- Wx123e4[1:(dim(x_train)[3]),((i-1)*hiddenstates*4+1+4):(i*hiddenstates*4-8)]
  Wxc <- Wx123e4[1:(dim(x_train)[3]),((i-1)*hiddenstates*4+1+8):(i*hiddenstates*4-4)]
  Wxo <- Wx123e4[1:(dim(x_train)[3]),((i-1)*hiddenstates*4+1+12):(i*hiddenstates*4)]
  
  Whi <- Wh123e4[,((i-1)*hiddenstates*4+1):(i*hiddenstates*4-12)]
  Whf <- Wh123e4[,((i-1)*hiddenstates*4+1+4):(i*hiddenstates*4-8)]
  Whc <- Wh123e4[,((i-1)*hiddenstates*4+1+8):(i*hiddenstates*4-4)]
  Who <- Wh123e4[,((i-1)*hiddenstates*4+1+12):(i*hiddenstates*4)]
  
  Bi <- B123e4[1:hiddenstates,i]
  Bf <- B123e4[(hiddenstates+1):(hiddenstates*2),i]
  Bc <- B123e4[(hiddenstates*2+1):(hiddenstates*3),i]
  Bo <- B123e4[(hiddenstates*3+1):(hiddenstates*4),i]
  
  Why <- Wy[,i]
  
  Bhy <- By[i]
  
  #for para criar os hiddenstates a partir da base de X.
  j<-1
  hmenos1 <- matrix(0,hiddenstates,1)
  ctmenos1 <- matrix(0,hiddenstates,1)
  states <- matrix(0,dim(X)[1],hiddenstates)
  yt1 <- matrix(0,dim(X),1)
  
  # FOR DENTRO DO FOR, PARA CALCULAR OS HT'S A PARTIR DAS MATRIZES DE PESO
  for(j in 1:(dim(X)[1])){
    input <- sigmoid(t(Whi)%*%hmenos1 + t(Wxi)%*%X[j,] + Bi)
    forget <- sigmoid(t(Whf)%*%hmenos1 + t(Wxf)%*%X[j,] + Bf)
    ctio <- tanh(t(Whc)%*%hmenos1 + t(Wxc)%*%X[j,] + Bc)
    output <- sigmoid(t(Who)%*%hmenos1 + t(Wxo)%*%X[j,] + Bo)
    ct <- forget*ctmenos1 + input*ctio
    ht <- output*tanh(ct)
    yt1[j] <- t(Why)%*%ht+Bhy
    
    states[j,] <- ht
    hmenos1 <- ht
    ctmenos1 <- ct
    
    htes[j,((i-1)*hiddenstates+1):(i*hiddenstates)] <- ht
    
  }
  
  states <- matrix(0,dim(X)[1],hiddenstates)
  yt2 <- matrix(0,dim(X),1)
  
  for(j in 1:(dim(X)[1])){
    input <- sigmoid(t(Whi)%*%hmenos1 + t(Wxi)%*%Z[j,] + Bi)
    forget <- sigmoid(t(Whf)%*%hmenos1 + t(Wxf)%*%Z[j,] + Bf)
    ctio <- tanh(t(Whc)%*%hmenos1 + t(Wxc)%*%Z[j,] + Bc)
    output <- sigmoid(t(Who)%*%hmenos1 + t(Wxo)%*%Z[j,] + Bo)
    ct <- forget*ctmenos1 + input*ctio
    ht <- output*tanh(ct)
    yt2[j] <- t(Why)%*%ht+Bhy
    
    states[j,] <- ht
    hmenos1 <- ht
    ctmenos1 <- ct
    
    htforecast[j,((i-1)*hiddenstates+1):(i*hiddenstates)] <- ht
    
    if(j == (dim(X)[1])){
      FORECASTS[i+forecasthorizon,7+(fh-1)*models] <- (t(Why)%*%ht+Bhy)*sigma2+mean2
    }else{    }
  

    ############## RF #################################
    #for (j in 1:3){
    #XRF <- embed(XeZ[-dim(XeZ)[1],],lags[j])
    #YRF <- scale(Yrwindow)
    #Y2RF <- scale(Y2window)
    #RF <- embed(XeZ[-1,],lags[j])
    
    
    #arbole <- matrix(0,dim(XRF)[1],dim(XRF)[2]+1)
    #arbole[,1] <- Y2RF[,-(lags[j]-1)]
    #arbole[,2:(dim(XRF)[2]+1)] <- XRF
    #arbole<-as.data.frame(arbole)
    
    #rf = randomForest(V1 ~.,data =arbole , ntree=500, nodesize = 10, importance = TRUE)
    
    #colnames(ZRF) <- colnames(arbole[,-1])
    #ZeRF <- ZRF[dim(ZRF)[1],]
    #ZeRF <- t(ZeRF)
    
    #predictpadronizado  <- predict(rf, as.data.frame(ZeRF))
    #FORECASTS[i+forecasthorizon-1+lags[j],(j+(models*(fh-1)))] <- predictpadronizado*sigma2+mean2
    #}
    }
  
  yt <- cbind(yt1,yt2)
  
k_clear_session()
}

toc()
save.image(file=paste0('CYCLE',forecasthorizon, 'fhorizon.RData'))
#load(file='LSTM12months2019-09-24.RData')
#Rodando RandomForest nos weights
i<-1
j<-1
dropout <- 12 #período a não ser considerado para realizar rf. Início começa com hiddenstates como vetor de zeros.
lags <- matrix(c(1,3,6),3,1)
hs<-1

# FOR COM OS RF'S NOS WEIGHTS CALCULADOS NO LSTM
for(j in 1:(dim(lags)[1])){
  
  for(hs in 1:(nforecast-forecasthorizon+1-lags[j])){
    ############## RF dos hidden states
    Yrwindow <- window(unemployment.ts,start=c(year(ymd(rollingwindow[hs+lags[j]+dropout-1,1])),month(rollingwindow[hs+lags[j]+dropout-1,1])),end=c(year(ymd(rollingwindow[hs,2])),month(rollingwindow[hs,2])))
    #Y2window <- as.numeric(window(delu.ts,start=c(year(ymd(rollingwindow[i+lags[j]+dropout,1])),month(rollingwindow[i+lags[j]+dropout,1])),end=c(year(ymd(rollingwindow[i,2])),month(rollingwindow[i,2]))))
    Y2window <- as.numeric(window(DELTAU.ts[,forecasthorizon+1],start=c(year(ymd(rollingwindow[hs+forecasthorizon+dropout+lags[j]-1,1])),month(rollingwindow[hs+forecasthorizon+dropout+lags[j]-1,1])),end=c(year(ymd(rollingwindow[hs,2])),month(rollingwindow[hs,2]))))
    
    ######################### Filtro HP #####################
    
    teste <- hpfilter(Yrwindow, type="lambda", freq = 14400)
    datas <- seq(from=as_date(rollingwindow[hs+lags[j]+dropout-1,1]), to=as_date(rollingwindow[hs,2]), by='month')
    df <- data.frame(abs=Yrwindow, z=as.data.frame(teste$cycle),Data=datas)
    #filtro <- ggplot(data=df, aes(x=Data,y=100*abs))+
    # geom_line()+
    # geom_line(linetype=2,aes(y=100*Series.1),color="red")
  
    Y2window <- teste$cycle-lag(teste$cycle,-forecasthorizon)
  
      ############################ FIltro HP ######################
    
    
    mean <- mean(Yrwindow)
    sigma <- sd(Yrwindow)
    mean2 <- mean(Y2window)
    sigma2<- sd(Y2window)
    
    htewindow <- htes[(dropout+forecasthorizon):(dim(htes)[1]),((hs-1)*4+1):(hs*4)]
    htfuturo <- htforecast[(dropout+forecasthorizon):(dim(htes)[1]),((hs-1)*4+1):(hs*4)]
    
    X <- embed(htewindow,lags[j])
    Y <- scale(Yrwindow)
    Y2<- scale(Y2window)
    Z <- embed(htfuturo,lags[j])
    
    arbole <- matrix(0,dim(X)[1],dim(X)[2]+1)
    arbole[,1] <- Y2
    arbole[,2:(dim(X)[2]+1)] <- X
    arbole<-as.data.frame(arbole)
    
    rf = randomForest(V1 ~.,data =arbole , ntree=500, nodesize = 10, importance = TRUE)
    
    colnames(Z) <- colnames(arbole[,-1])
    Ze <- Z[dim(Z)[1],]
    Ze <- t(Ze)
    
    predictpadronizado  <- predict(rf, as.data.frame(Ze))
    FORECASTS [hs+forecasthorizon,(j+3)+(fh-1)*models]  <- predictpadronizado*sigma2+mean2
    print(hs)
  }
  
}

}

finaldate <- mccrack[dim(mccrack)[1]-1,1]
firstdate <- mccrack[dim(mccrack)[1]-dim(FORECASTS)[1],1]
FORECASTS.ts <- ts(FORECASTS,start=c(year(firstdate),month(firstdate)),end=c(year(finaldate),month(finaldate)),frequency=12)

# COMPARAÇÃO DOS RESULTADOS
compara <- na.omit(cbind(DELTAU.ts,FORECASTS.ts))

# 1 forecast
bestrf <- matrix(0,dim(as.matrix(horizonvector))[1],1)
bestdl <- matrix(0,dim(as.matrix(horizonvector))[1],1)
for(fh in 1:dim(as.matrix(horizonvector))[1]){
  
forecasthorizon <- horizonvector[fh]

MSERF <- colMeans((compara[,(14+(fh-1)*8):(16+(fh-1)*8)]-compara[,forecasthorizon+1])^2)
MSEDL <- colMeans((compara[,(17+(fh-1)*8):(21+(fh-1)*8)]-compara[,forecasthorizon+1])^2)

assign(paste0("graphdata",forecasthorizon, "horizon"),cbind(compara[,forecasthorizon+1],compara[,(13+which.min(MSERF)+(fh-1)*8)],compara[,(16+which.min(MSEDL)+(fh-1)*8)]))
assign(paste0("MSERF",forecasthorizon,"h"),MSERF)
assign(paste0("MSEDL",forecasthorizon,"h"),MSEDL)

bestrf[fh] <- which.min(MSERF)
bestdl[fh] <- which.min(MSEDL)

}

i <- 1
j <- 1

for(fh in 1:dim(as.matrix(horizonvector))[1]){
  forecasthorizon <- horizonvector[fh]
  ######################### INÍCIO DO "FOR" COM OS MODELOS RF's ######################################
  for(j in 1:(dim(lags)[1])){
    
  for(i in 1:((nforecast-forecasthorizon))){
    
############## RF da base de dados total
Xrwindow <- t(na.omit(t(window(mccrack.ts, start=c(year(ymd(rollingwindow[i,1])),month(rollingwindow[i,1])),end=c(year(ymd(rollingwindow[i+1-forecasthorizon,2])),month(rollingwindow[i+1-forecasthorizon,2]))))))
Yrwindow <- window(unemployment.ts,start=c(year(ymd(rollingwindow[i+lags[j]-1,1])),month(rollingwindow[i+lags[j]-1,1])),end=c(year(ymd(rollingwindow[i,2])),month(rollingwindow[i,2])))
Y2window <- as.numeric(window(DELTAU.ts[,forecasthorizon+1],start=c(year(ymd(rollingwindow[(i+forecasthorizon-1+lags[j]),1])),month(rollingwindow[(i+lags[j]+forecasthorizon-1),1])),end=c(year(ymd(rollingwindow[(i),2])),month(rollingwindow[(i),2]))))
XeZ <- scale(Xrwindow)

######################### Filtro HP #####################

teste <- hpfilter(Yrwindow, type="lambda", freq = 14400)
datas <- seq(from=as_date(rollingwindow[i+lags[j]-1,1]), to=as_date(rollingwindow[i,2]), by='month')
df <- data.frame(abs=Yrwindow, z=as.data.frame(teste$cycle),Data=datas)
#filtro <- ggplot(data=df, aes(x=Data,y=100*abs))+
# geom_line()+
# geom_line(linetype=2,aes(y=100*Series.1),color="red")

Y2window <- teste$cycle-lag(teste$cycle,-forecasthorizon)

############################ FIltro HP ######################



mean <- mean(Yrwindow)
sigma <- sd(Yrwindow)
mean2 <- mean(Y2window)
sigma2<- sd(Y2window)

X <- embed(XeZ[-dim(XeZ)[1],],lags[j])
Y <- scale(Yrwindow)
Y2<- scale(Y2window)
Z <- embed(XeZ[-1,],lags[j])

arbole <- matrix(0,dim(X)[1],dim(X)[2]+1)
arbole[,1] <- Y2
arbole[,2:(dim(X)[2]+1)] <- X
arbole<-as.data.frame(arbole)

rf = randomForest(V1 ~.,data =arbole , ntree=500, nodesize = 10, importance = TRUE)

colnames(Z) <- colnames(arbole[,-1])
Ze <- Z[dim(Z)[1],]
Ze <- t(Ze)

predictpadronizado  <- predict(rf, as.data.frame(Ze))
FORECASTS [i+forecasthorizon,(j+(models*(fh-1)))] <- predictpadronizado*sigma2+mean2
############## Fim do RF da base de dados total

XAR <- embed(Y2window,lags[j]+forecasthorizon)[,-(1:forecasthorizon)]
Y2AR<- Y2window[-(1:(lags[j]-1+forecasthorizon))]
ZAR <- embed(Y2window,lags[j])[(dim(as.matrix(Y2window))[1]+1-lags[j]),]

ARmodel <- lm(Y2AR ~ XAR)
FORECASTS [i+forecasthorizon,(8+j+(models*(fh-1)))] <- c(1,t(ZAR))%*%coef(ARmodel)


print(i)
  }
  }
}


colnames(graphdata1horizon) <- c("REAL",paste0("RF",bestrf[1],"lags"),paste0(bestdl[1],"model")) 
colnames(graphdata3horizon) <- c("REAL",paste0("RF",bestrf[2],"lags"),paste0(bestdl[2],"model")) 
colnames(graphdata6horizon) <- c("REAL",paste0("RF",bestrf[3],"lags"),paste0(bestdl[3],"model"))
colnames(graphdata12horizon) <- c("REAL",paste0("RF",bestrf[4],"lags"),paste0(bestdl[4],"model"))
colnames(graphdata24horizon) <- c("REAL",paste0("RF",bestrf[5],"lags"),paste0(bestdl[5],"model"))

graph1hdt <- data.frame(mês=seq(firstdate,finaldate, by = "month"),REAL=graphdata1horizon[,1],RF=graphdata1horizon[,2],DL=graphdata1horizon[,3])
graph1h <- ggplot(data=graph1hdt, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")

graph3hdt <- data.frame(mês=seq(firstdate,finaldate, by = "month"),REAL=graphdata3horizon[,1],RF=graphdata3horizon[,2],DL=graphdata3horizon[,3])
graph3h <- ggplot(data=graph3hdt, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")

graph6hdt <- data.frame(mês=seq(firstdate,finaldate, by = "month"),REAL=graphdata6horizon[,1],RF=graphdata6horizon[,2],DL=graphdata6horizon[,3])
graph6h <- ggplot(data=graph6hdt, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")

graph12hdt <- data.frame(mês=seq(firstdate,finaldate, by = "month"),REAL=graphdata12horizon[,1],RF=graphdata12horizon[,2],DL=graphdata12horizon[,3])
graph12h <- ggplot(data=graph12hdt, aes(x=mês, y=REAL))+
  geom_line()+
  geom_line(linetype=1,aes(y=RF),color="orange")+
  geom_line(linetype=1,aes(y=DL),color="blue")


save.image(file=paste0('HSrfcomplete', nforecast,'cycleforecasts.RData'))

end_time <- Sys.time()
end_time - start_time #2h30min
#############################################################