paquetes<-c("quantmod", "PerformanceAnalytics", "readxl", "httr")

for(i in paquetes){
  if(!require(i, character.only = TRUE)){
    install.packages(i, dependencies=TRUE)
  }
  require(i, character.only = TRUE)  
}

acciones_arg <- c("ALUA.BA",                   "BMA.BA",                  "BYMA.BA",
                  "CEPU.BA",                  "COME.BA",                  "CRES.BA",                  "CVH.BA",
                  "EDN.BA",                  "GGAL.BA",                  "MIRG.BA",                  "PAMP.BA",
                  "SUPV.BA",                  "TECO2.BA",                  "TGNO4.BA",                  "TGSU2.BA",
                  "TRAN.BA",                  "VALO.BA",                  "YPFD.BA", 
                  "AGRO.BA","AUSO.BA","BBAR.BA","BHIP.BA","BOLT.BA","BPAT.BA","BRIO.BA","CADO.BA","CAPX.BA",
                  "CECO2.BA","CELU.BA","CGPA2.BA","CTIO.BA","DGCU2.BA","DOME.BA","DYCA.BA","ESME.BA",
                  "FERR.BA","FIPL.BA","GAMI.BA","GARO.BA","GBAN.BA","GCLA.BA","GRIM.BA","HARG.BA","HAVA.BA",
                  "INTR.BA","INVJ.BA","IRCP.BA","IRSA.BA","LEDE.BA","LOMA.BA","LONG.BA","METR.BA","MOLA.BA",
                  "MOLI.BA","MORI.BA","OEST.BA","PATA.BA","PGR.BA","POLL.BA","RIGO.BA","ROSE.BA",
                  "SAMI.BA","SEMI.BA","TGLT.BA")

dif_acciones_arg <- data.frame()

for (i in 1:length(acciones_arg)){
  #obtengo los precios
  accion <- getSymbols(acciones_arg[i], from = "2017-12-26", 
                     to = "2020-05-15", auto.assign = FALSE)
  #me quedo con el cierre
  data<-accion[,4]
  # #calculo MACD
  macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
  macd$dif <- macd$macd - macd$signal
  colnames(macd) <- c("macd", "signal", acciones_arg[i])
  dif_acciones_arg <- cbind(dif_acciones_arg,macd[,3])
  
}


for (i in 1:ncol(dif_acciones_arg)){
  print(colnames(dif_acciones_arg[,i]))
  
  print(table(is.na(dif_acciones_arg[,i])))
  }




salud <- c("ABBV","MRK","JNJ","AZN","GSK","AGN","LLY","SNY")

dif_salud <- data.frame()

for (i in 1:length(salud)){
  #obtengo los precios
  accion <- getSymbols(salud[i], from = fecha_desde_analisis, 
                       to = fecha_hasta, auto.assign = FALSE)
  #me quedo con el cierre
  data<-accion[,4]
  table(is.na(data))
  i <- i + 1
  
  #calculo MACD
  macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
  macd$dif <- macd$macd - macd$signal
  colnames(macd) <- c("macd", "signal", salud[i])
  dif_salud <- cbind(dif_salud,macd[,3])
}


#############################   ETF INVERSE   ######################################

etf_inverse <- c("DOG","DXD","PSQ","SDOW","QID","SPXS","SH","SPXU","SDS","SQQQ")


dif_etf_inverse <- data.frame()

for (i in 1:length(etf_inverse)){
  #obtengo los precios
  accion <- getSymbols(etf_inverse[i], from = "2015-01-01", 
                       to = "2020-04-27", auto.assign = FALSE)
  #me quedo con el cierre
  data<-accion[,4]
  #calculo MACD
  macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
  macd$dif <- macd$macd - macd$signal
  colnames(macd) <- c("macd", "signal", etf_inverse[i])
  dif_etf_inverse <- cbind(dif_etf_inverse,macd[,3])
}

largo = length(etf_inverse)
for (z in 1:largo){
  grafico <- plot(dif_etf_inverse["2020-01-01/2020-04-30",z], main = etf_inverse[z], las = 0)
  print(grafico)
}



################################### FIN ETF INVERSE #############################

#############################   ETF HEALTH CARE   ######################################

etf_health <- c("XLV", "XBI", "IBB", "VHT", "IHI", "FHLC", "FXH", "BIB", "CURE", "FBT", "BIS", "XPH")


dif_etf_health <- data.frame()

for (i in 1:length(etf_health)){
  #obtengo los precios
  accion <- getSymbols(etf_health[i], from = "2015-01-01", 
                       to = "2020-04-27", auto.assign = FALSE)
  #me quedo con el cierre
  data<-accion[,4]
  #calculo MACD
  macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
  macd$dif <- macd$macd - macd$signal
  colnames(macd) <- c("macd", "signal", etf_health[i])
  dif_etf_health <- cbind(dif_etf_health,macd[,3])
}

largo = length(etf_health)
for (z in 1:largo){
  grafico <- plot(dif_etf_health["2020-01-01/2020-04-30",z], main = etf_health[z], las = 0)
  print(grafico)
}



################################### FIN ETF HEALTH CARE #############################



#obtengo los precios de ^NSEI
getSymbols("AAPL")
#grafico
chartSeries(AAPL, TA=NULL)
#de todos los precios me quedo con el de cierre
data<-AAPL[,4]
#calculo MACD
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
#grafico MACD
chartSeries(AAPL, TA="addMACD()")

signal <- Lag(ifelse(macd$macd < macd$signal, -1, 1))

returns <- ROC(data)*signal

returns <- returns['2008-06-02/2015-09-22']

portfolio <- exp(cumsum(returns))

plot(portfolio)
table.Drawdowns(ret, top=10)

table.DownsideRisk(returns)

charts.PerformanceSummary(returns)

etf_inverse <- c("AAPL", "PBR", "MELI", "SPY")

total_cedear<- lapply(etf_inverse, getSymbols, auto.assign = FALSE)

for (i in 1:length(total_acciones))
{
  total_acciones[[i]] <- total_acciones[[i]]["2017-08-30/2020-04-30"]
}
View(total_acciones)

install.packages("corrplot")
library(corrplot)
corrplot(cor(total_acciones), method="circle")



#no se porque mierda no baja los Cedear, entonces tomo los de mayor volumen
#defino parámetro de fechas
fechas <- "2020-01-01/2020-04-29"

url <- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEAGOOGL&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "GOOGLE", las = 2)

url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARPBR&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "PBR")

url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARGOLD&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "GOLD")


url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARBBD&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "BBD")


url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARVALE&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "VALE")


url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARKO&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "KO")


url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARGE&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "GE")

url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARMSFT&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "MSFT")

url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARAUY&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "AUY")

url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARAAPL&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "AAPL")

url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEARHMY&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "HMY")

url	<- ("http://www.rava.com/empresas/precioshistoricos.php?e=CEDEAROGZD&csv=1")
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
data <- read.csv(tf)
date <- as.Date(as.character(data$fecha), "%Y-%m-%d")
mktCap <- as.numeric(data$cierre)
data <- xts(mktCap, date)
macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
macd$dif <- macd$macd - macd$signal
plot(macd$dif[fechas], main = "OGZD")
