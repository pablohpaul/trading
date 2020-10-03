library(TTR)
library(scales)
library(quantmod)
library(httr)
library(xlsx)
library(lubridate)
library(XLConnect)


#########################################################
tickers <- read.xlsx("D:/Descargas/Balance IOL.xlsx", sheetName="EMA")

acciones_del_dia <- function(tickers){  
  del_dia <- data.frame()

  for (w in 1:nrow(tickers)){
  
    ticker <- tickers$ticker[w]
    
      try({
      #traigo precio de las acciones
      cotizaciones <- getSymbols(ticker, auto.assign = FALSE, from ="2019-01-01")
      #Cambio nombres columnas
      colnames(cotizaciones) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
      #saco los valores nulos
      cotizaciones <- cotizaciones[!is.na(cotizaciones$Close),]
    
      #calculo MACD y lo agrego a cotizaciones
      macd <- MACD(cotizaciones$Close, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
      cotizaciones <- merge(cotizaciones, macd)
    
      #calculo EMA y agrego
      ema_corto <- EMA(cotizaciones$Close, n=tickers[w,3])
      ema_largo <- EMA(cotizaciones$Close, n=tickers[w,4])
      cotizaciones <- merge(cotizaciones, ema_corto, ema_largo)
    
      #calculo estocastico
      estocastico <- stoch(cotizaciones[,c("High", "Low", "Close")], nFastK = 14, nFastD = 3, nSlowD = 6)
      cotizaciones <- merge(cotizaciones, estocastico)
    
      #Cambio nombres columnas
      colnames(cotizaciones) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "MACD", "Signal", "EMA_corto", "EMA_larga", "STO_fastK", "STO_fastD", "STO_slowD")
      
      accion <- cotizaciones
    
      #señal 0 = no opera - C = compra - V = venta
      accion$senal <- "0"
      accion$stop_loss <- 0
      ultimaOper <- "Ve"
      accion$rendimiento <- 0
      precioCompra <- 0
      stopLoss <- 0
    
      #para todas las filas
      for (i in 1:nrow(accion)){
        operoHoy <- "No"
        #si tengo una compra vigente y toca el stop loss entonces vendo
        if ((ultimaOper == "Co") & (operoHoy == "No")){
          if (as.numeric(accion$Low[i]) < stopLoss){
            accion$senal[i] <- "Ve"
            ultimaOper <- "Ve"
            accion$rendimiento[i] <- (as.numeric(accion$Low[i]) * 100 / as.numeric(precioCompra)) - 100
            precioCompra <- 0
            operoHoy <- "Si"
          }
        }
        # #si el estocástico sube de 30 y la última operación no es compra
        # if (as.numeric(accion$STO_fastK[i]) > 0.30 & !is.na(accion$STO_fastK[i]) & ultimaOper != "Co" & operoHoy == "No"){
        #   #si el estocástico anterior fue menor a 30 (implica q cruza hacia arriba la linea de 30) => compro
        #   if (accion$STO_fastK[i-1] < 0.30 & !is.na(accion$STO_fastK[i-1])){
        #     accion$senal[i] <- "Co"
        #     ultimaOper <- "Co"
        #     accion$stop_loss[i] <- as.numeric(accion$Close[i]) * 0.95
        #     precioCompra <- accion$Close[i]
        #     stopLoss <- as.numeric(accion$stop_loss[i])
        #     operoHoy <- "Si"
        #   }
        #si el cruce de rápida es hacia arriba y la última operación no es compra
        if (operoHoy == "No" & !is.na(accion$EMA_corto[i]) & !is.na(accion$EMA_larga[i]) & as.numeric(accion$EMA_corto[i]) > as.numeric(accion$EMA_larga[i]) & ultimaOper != "Co"){
          #si la anterior ema en corto fue menor (implica que la cruza a la larga hacia arriba) => compro
          if (as.numeric(accion$EMA_corto[i-1]) < as.numeric(accion$EMA_larga[i-1]) & !is.na(accion$EMA_corto[i-1]) & !is.na(accion$EMA_larga[i-1]) ){
            accion$senal[i] <- "Co"
            ultimaOper <- "Co"
            accion$stop_loss[i] <- as.numeric(accion$Close[i]) * 0.95
            precioCompra <- accion$Close[i]
            stopLoss <- as.numeric(accion$stop_loss[i])
            operoHoy <- "Si"
          }
        #si la EMA no es nula y la corta es menor que la larga y la última operación no fue venta
        } else if (!is.na(accion$EMA_corto[i]) & !is.na(accion$EMA_larga[i]) & as.numeric(accion$EMA_corto[i]) < as.numeric(accion$EMA_larga[i]) & ultimaOper != "Ve" & operoHoy == "No"){
          #si la anterior ema en corto fue mayor (implica que la cruza a la larga hacia abajo) => vendo
          if ( as.numeric(accion$EMA_corto[i-1]) > as.numeric(accion$EMA_larga[i-1]) & !is.na(accion$EMA_corto[i-1]) & !is.na(accion$EMA_larga[i-1]) ){
            accion$senal[i] <- "Ve"
            ultimaOper <- "Ve"
            accion$rendimiento[i] <- (as.numeric(accion$Close[i]) * 100 / as.numeric(precioCompra)) - 100
            precioCompra <- 0
            operoHoy <- "Si"
          }
        }
        assign(paste0("rendimiento_", tickers$ticker[w]), data.frame(accion))
      }
    }, silent = TRUE)
    del_dia <- rbind(del_dia, accion[nrow(accion),])
    del_dia$Adjusted[w] <- tickers$ticker[w]
    colnames(del_dia) <- colnames(accion)
  
  }

View(del_dia)

}

#########################################################

calculo_inversion <- function(matriz){
  montoInicial <- 10000
  comision <- 0.005
  iva <- 0.21
  primera <- "si"
  
  matriz <- data.frame(date=index(matriz), coredata(matriz))
  matriz <- matriz[matriz$senal != 0, ]
  
  matriz$Close <- as.numeric(matriz$Close)
  
  matriz$cantidad <- 0
  matriz$invertido <- 0
  matriz$comision <- 0
  matriz$iva <- 0
  matriz$saldo <- 0
  
  for (k in 1:nrow(matriz))
  {
    if (matriz$senal[k] == "Co"){
      if (primera == "si"){
        matriz$cantidad[k] <- trunc(montoInicial / matriz$Close[k])
        matriz$invertido[k] <- matriz$cantidad[k] * matriz$Close[k]
        matriz$comision[k] <- matriz$invertido[k] * comision
        matriz$iva[k] <- matriz$comision[k] * iva
        matriz$saldo[k] <- montoInicial - matriz$invertido[k] - matriz$comision[k] - matriz$iva[k]
        primera <- "no"
      } else {
        matriz$cantidad[k] <- trunc(matriz$saldo[k-1] / matriz$Close[k])
        matriz$invertido[k] <- matriz$cantidad[k] * matriz$Close[k]
        matriz$comision[k] <- matriz$invertido[k] * comision
        matriz$iva[k] <- matriz$comision[k] * iva
        matriz$saldo[k] <- matriz$saldo[k-1] - matriz$invertido[k] - matriz$comision[k] - matriz$iva[k]
      }
    } else if (matriz$senal[k] == "Ve"){
      matriz$cantidad[k] <- matriz$cantidad[k-1]
      matriz$invertido[k] <- matriz$cantidad[k-1] * matriz$Close[k]
      matriz$comision[k] <- matriz$invertido[k] * comision
      matriz$iva[k] <- matriz$comision[k] * iva
      matriz$saldo[k] <- matriz$saldo[k-1] + matriz$invertido[k] - matriz$comision[k] - matriz$iva[k]
    }
  }
  #si la última operacion es compra la borro para calcular bien el rendimiento y no dejar una ope abierta
  if (matriz$senal[nrow(matriz)] == "Co"){
    matriz <- matriz[-nrow(matriz),]
  }
  
  diferencia <- (matriz$saldo[nrow(matriz)] / matriz$invertido[1]) -1
  cat(paste("Rendimiento", percent(diferencia, accuracy = 0.01), sep = "\n"))
  matriz
}

#################################################
###VERIFICA RENDIMIENTO DE LAS ACCIONES DE USA###
### CRUCE DE EMA y STOCASTICO####################
#################################################

tickersList <- stockSymbols()

tickerList <- promedios$Symbol

promedios <- data.frame()

for (j in 1:nrow(tickersList)){
  
  #nombre de la accion
  ticker <- tickersList[j,1]
  try({  
    #traigo precio de las acciones
    ggal <- getSymbols(ticker, auto.assign = FALSE, from="2019-01-01")
    
  
    #Cambio nombres columnas
    colnames(ggal) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
    #saco los valores nulos
    ggal <- ggal[!is.na(ggal$Close),]
  
    #calculo MACD y lo agrego a ggal
    macd <- MACD(ggal$Close, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
    ggal <- merge(ggal, macd)
  
    #calculo EMA y agrego
    ema_corto <- EMA(ggal$Open, n=8)
    ema_largo <- EMA(ggal$Open, n=40)
    ggal <- merge(ggal, ema_corto, ema_largo)
    
    #calculo estocastico
    estocastico <- stoch(ggal[,c("High", "Low", "Close")], nFastK = 14, nFastD = 3, nSlowD = 6)
    ggal <- merge(ggal, estocastico)
    
    #Cambio nombres columnas
    colnames(ggal) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "MACD", "Signal", "EMA_corto", "EMA_larga", "STO_fastK", "STO_fastD", "STO_slowD")
    
    accion <- ggal
  
    #señal 0 = no opera - C = compra - V = venta
    accion$senal <- "0"
    accion$stop_loss <- 0
    ultimaOper <- "Ve"
    accion$rendimiento <- 0
    precioCompra <- 0
    stopLoss <- 0
  
    #para todas las filas
    for (i in 1:nrow(accion)){
      operoHoy <- "No"
      #si tengo una compra vigente y toca el stop loss entonces vendo
      if ((ultimaOper == "Co") & (operoHoy == "No")){
        if (as.numeric(accion$Low[i]) < stopLoss){
          accion$senal[i] <- "Ve"
          ultimaOper <- "Ve"
          accion$rendimiento[i] <- (as.numeric(accion$Low[i]) * 100 / as.numeric(precioCompra)) - 100
          precioCompra <- 0
          operoHoy <- "Si"
        }
      }
      #si el estocástico sube de 30 y la última operación no es compra
      if (as.numeric(accion$STO_fastK[i]) > 0.30 & !is.na(accion$STO_fastK[i]) & ultimaOper != "Co" & operoHoy == "No"){
        #si el estocástico anterior fue menor a 30 (implica q cruza hacia arriba la linea de 30) => compro
        if (accion$STO_fastK[i-1] < 0.30 & !is.na(accion$STO_fastK[i-1])){
          accion$senal[i] <- "Co"
          ultimaOper <- "Co"
          accion$stop_loss[i] <- as.numeric(accion$Close[i]) * 0.95
          precioCompra <- accion$Close[i]
          stopLoss <- as.numeric(accion$stop_loss[i])
          operoHoy <- "Si"
        }
      #si la EMA no es nula y la corta es menor que la larga y la última operación no fue venta
      } else if (!is.na(accion$EMA_corto[i]) & !is.na(accion$EMA_larga[i]) & as.numeric(accion$EMA_corto[i]) < as.numeric(accion$EMA_larga[i]) & ultimaOper != "Ve" & operoHoy == "No"){
        #si la anterior ema en corto fue mayor (implica que la cruza a la larga hacia abajo) => vendo
        if ( as.numeric(accion$EMA_corto[i-1]) > as.numeric(accion$EMA_larga[i-1]) & !is.na(accion$EMA_corto[i-1]) & !is.na(accion$EMA_larga[i-1]) ){
          accion$senal[i] <- "Ve"
          ultimaOper <- "Ve"
          accion$rendimiento[i] <- (as.numeric(accion$Close[i]) * 100 / as.numeric(precioCompra)) - 100
          precioCompra <- 0
          operoHoy <- "Si"
        }
      }
    }
  
    #summary(as.numeric(accion$rendimiento))
  
    cantVentas <- table(accion$senal[accion$senal == "Ve"])[[1]]
    totalRend <- sum(as.numeric(accion$rendimiento))
    
    promedios[j,1] <- tickersList[j,1]
    promedios[j,2] <- totalRend / cantVentas
    promedios[j,3] <- totalRend
  }, silent=TRUE)
}

colnames(promedios) <- c("Symbol", "Promedio", "Rend_Total")

promedios <- merge(promedios, tickersList[,1:2], by="Symbol")

promedios <- promedios[order(-promedios$Rend_Total),]

promedios$ranking <- seq(1,nrow(promedios),1)

View(promedios)

################################
######CHEQUEO MEJOR CRUCE#######
######SOLO CRUCE DE MEDIAS######
################################


#parámetros
rapida_ini = 2
rapida_fin = 24
lenta_ini = 18
lenta_fin = 60
intervalo = 2

#traigo precio de las acciones
fechas <- c("2019-01-01","2018-01-01","2015-01-01")
lista_tickers <- c("TAN", "ICLN")

for (p in 1:length(lista_tickers)){
  for (z in 1:3){
  rendimiento <- data.frame()
  
  fila <-1
  
  ticker <- lista_tickers[p]
  intervalo = 2
  
  
  cotizaciones <- getSymbols(ticker, auto.assign = FALSE, from = fechas[z])
  #Cambio nombres columnas
  colnames(cotizaciones) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  #saco los valores nulos
  cotizaciones <- cotizaciones[!is.na(cotizaciones$Close),]
  
  
  for (verde in seq(rapida_ini,rapida_fin,intervalo)){
  for (rojo in seq(lenta_ini,lenta_fin,intervalo)){
    try({  
      accion <- cotizaciones
      
      #calculo EMA y agrego
      ema_corto <- EMA(accion$Open, n=verde)
      ema_largo <- EMA(accion$Open, n=rojo)
      accion <- merge(accion, ema_corto, ema_largo)
      
      #calculo estocastico
      #estocastico <- stoch(ggal[,c("High", "Low", "Close")], nFastK = 14, nFastD = 3, nSlowD = 6)
      #ggal <- merge(ggal, estocastico)
      
      #Cambio nombres columnas
      colnames(accion) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "EMA_corto", "EMA_larga")
      
      #señal 0 = no opera - C = compra - V = venta
      accion$senal <- "0"
      accion$stop_loss <- 0
      ultimaOper <- "Ve"
      accion$rendimiento <- 0
      precioCompra <- 0
      stopLoss <- 0
      
      #para todas las filas
      for (i in 1:nrow(accion)){
        operoHoy <- "No"
        #si tengo una compra vigente y toca el stop loss entonces vendo
        if (ultimaOper == "Co" & operoHoy == "No"){
          if (as.numeric(accion$Low[i]) < stopLoss){
            accion$senal[i] <- "Ve"
            ultimaOper <- "Ve"
            accion$rendimiento[i] <- (as.numeric(accion$Low[i]) * 100 / as.numeric(precioCompra)) - 100
            precioCompra <- 0
            operoHoy <- "Si"
          }
        }
        #si el cruce de rápida es hacia arriba y la última operación no es compra
        if (operoHoy == "No" & !is.na(accion$EMA_corto[i]) & !is.na(accion$EMA_larga[i]) & as.numeric(accion$EMA_corto[i]) > as.numeric(accion$EMA_larga[i]) & ultimaOper != "Co"){
          #si la anterior ema en corto fue menor (implica que la cruza a la larga hacia arriba) => compro
          if (as.numeric(accion$EMA_corto[i-1]) < as.numeric(accion$EMA_larga[i-1]) & !is.na(accion$EMA_corto[i-1]) & !is.na(accion$EMA_larga[i-1]) ){
            accion$senal[i] <- "Co"
            ultimaOper <- "Co"
            accion$stop_loss[i] <- as.numeric(accion$Close[i]) * 0.95
            precioCompra <- accion$Close[i]
            stopLoss <- as.numeric(accion$stop_loss[i])
            operoHoy <- "Si"
          }
          #si la EMA no es nula y la corta es menor que la larga y la última operación no fue venta
        } else if (operoHoy == "No" & !is.na(accion$EMA_corto[i]) & !is.na(accion$EMA_larga[i]) & as.numeric(accion$EMA_corto[i]) < as.numeric(accion$EMA_larga[i]) & ultimaOper != "Ve"){
          #si la anterior ema en corto fue mayor (implica que la cruza a la larga hacia abajo) => vendo
          if ( as.numeric(accion$EMA_corto[i-1]) > as.numeric(accion$EMA_larga[i-1]) & !is.na(accion$EMA_corto[i-1]) & !is.na(accion$EMA_larga[i-1]) ){
            accion$senal[i] <- "Ve"
            ultimaOper <- "Ve"
            accion$rendimiento[i] <- (as.numeric(accion$Close[i]) * 100 / as.numeric(precioCompra)) - 100
            precioCompra <- 0
            operoHoy <- "Si"
          }
        }
      }
      
      #summary(as.numeric(accion$rendimiento))
      
      cantVentas <- sum(accion$senal == "Ve")
      totalRend <- sum(as.numeric(accion$rendimiento))
      
      rendimiento[fila, 1] <- verde
      rendimiento[fila, 2] <- rojo
      rendimiento[fila, 3] <- cantVentas
      rendimiento[fila, 4] <- totalRend
      rendimiento[fila, 5] <- totalRend / cantVentas
      rendimiento[fila, 6] <- sum(accion$rendimiento > 0)
      rendimiento[fila, 7] <- sum(accion$rendimiento < 0)
      fila = fila + 1
      
    }, silent=TRUE)
      colnames(rendimiento) <- c("Fast", "Slow", "Cant_Oper", "total_Rend", "Media", "Positivas", "Negativas")
  }
  }
  
  assign(paste0("rendimiento_", ticker,"_", year(as.Date(fechas[z])) ), data.frame(rendimiento))
  
  }
}
