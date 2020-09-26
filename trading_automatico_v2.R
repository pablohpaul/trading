library(TTR)
library(quantmod)
library(httr)


#defino parámetro de fechas
fecha_desde_analisis <- "2018-12-26"
fecha_desde <- "2019-01-01"
fecha_hasta <- "2020-06-19"
fecha_hasta_menos1 <- "2020-06-18"

fechas <- paste0(fecha_desde, "/", fecha_hasta)


acciones_arg <- c("ALUA.BA","BMA.BA", "BYMA.BA", "CEPU.BA", "COME.BA",  "CRES.BA", "CVH.BA",
                 "EDN.BA", "GGAL.BA", "MIRG.BA", "PAMP.BA", "SUPV.BA", "TECO2.BA", "TGNO4.BA", "TGSU2.BA",
                 "TRAN.BA", "VALO.BA", "YPFD.BA", "AGRO.BA","AUSO.BA","BHIP.BA","BOLT.BA","BPAT.BA",
                 "BRIO.BA","CADO.BA","CAPX.BA", "CECO2.BA","CELU.BA","CGPA2.BA","CTIO.BA","DGCU2.BA",
                 "DOME.BA","DYCA.BA","ESME.BA", "FERR.BA","FIPL.BA","GARO.BA","GBAN.BA","GCLA.BA","GRIM.BA",
                 "HAVA.BA", "INTR.BA","INVJ.BA","IRCP.BA","IRSA.BA","LEDE.BA","LOMA.BA","LONG.BA","METR.BA",
                 "MOLA.BA", "MOLI.BA","MORI.BA","OEST.BA","PATA.BA","POLL.BA","RIGO.BA","ROSE.BA",
                 "SAMI.BA","SEMI.BA","TGLT.BA")

diferencias_arg <- data.frame()
cotizaciones <- data.frame()

acciones

acciones_arg <- "EDN.BA"


for (i in 1:length(acciones_arg)){
  #obtengo los precios
  accion <- getSymbols(acciones_arg[i], from = fecha_desde_analisis, 
                       to = fecha_hasta, auto.assign = FALSE)
  #me quedo con el cierre
  data<-accion[,4]
  cotizaciones <- cbind(cotizaciones, data)
  
  #calculo MACD
  macd <- MACD(data, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)
  macd <- cbind(macd, data)
  macd$dif <- macd$macd - macd$signal
  colnames(macd) <- c("macd", "signal", acciones_arg[i])
  diferencias_arg <- cbind(diferencias_arg,macd[,3])

}

macd$oper <- ''
macd$precio_oper <- 0




acciones_arg <- c("ALUA.BA","BMA.BA")


for (i in 1:length(acciones_arg)){

stock <- acciones_arg[i]  

cotiza <- getSymbols("AAPL", auto.assign = FALSE)

data=cotiza[,4]

macd = MACD(data, nFast=12, nSlow=26,nSig=9,maType=SMA,percent = FALSE)

chartSeries(data['2020-01-01/2020-05-28'], TA="addMACD()", type="candlesticks"  , name = stock)
}




cotiza <- getSymbols("AAPL", auto.assign = FALSE)
data=cotiza[,4]
macd = MACD(data, nFast=12, nSlow=26,nSig=9,maType=SMA,percent = FALSE)
chartSeries(cotiza,  type="candlestick", subset='2020', TA="addMACD()", name="AAPL")


macd <- macd(subset='2020')

signal <- Lag(ifelse(macd$macd < macd$signal, -1, 1))
returns <- ROC(data)*signal
portfolio <- exp(cumsum(returns))
plot(portfolio)





arg <- as.data.frame(t(diferencias_arg[fecha_hasta_menos1, ]))
colnames(arg) <- c("Valor")
arg$Nombre <- rownames(arg)
arg$señal <- 1
arg$señal[arg$Valor > 0.15] <- 0
arg$señal[arg$Valor < -0.15] <- 0

View(arg[arg$señal == 1, ])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(m, method="color", col=col(200),  
         type="upper", order="hclust", 
         tl.col="black", tl.srt=45, tl.cex = 0.35, #text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
