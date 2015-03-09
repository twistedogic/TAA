#' TAA
#' 
#' This is a wrapper for the OpenCPU application. It is a single function that calls out to various plot types.
#' The function prints data.
#' 
#' @param id stock ticker symbol. E.g. "GOOG".
#' @param endPoint query endpoint which output csv
#' @import quantmod
#' @import TTR
#' @import RJSONIO
#' @import xts
#' @export
TAA <- function(id='0001.HK', endPoint = 'http://10.0.0.114:3000/api/hist/desc/'){
  url <- paste(endPoint,id,sep = '')
  DF <- read.csv(url)
  res <- DF[apply(DF[c(3:7)],1,function(z) !any(z==0)),]
  data <- as.xts(res[,3:7],order.by=as.Date(res$Date),unique=T)
  checkNull <- as.numeric(Cl(data))
  if (length(na.omit(checkNull)) > 250){
    o <- as.vector(data[,1])
    tatitle <- colnames(data)
    CL <- Cl(data)
    HLC <- HLC(data)
    HL <- data[,c(2,3)]
    V <- data[,5]
    change <- ROC(CL) #change
    tatitle <- c(tatitle,c('change'))
    rsi <- RSI(CL) #rsi
    tatitle <- c(tatitle,c('rsi'))
    sto <- stoch(HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType = 'EMA') * 100 #FastK,FastD,SlowD
    tatitle <- c(tatitle,names(sto))
    smi <- SMI(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9) #smi,smiSignal
    tatitle <- c(tatitle,c('smi','smiSignal'))
    macd <- MACD(CL, nFast = 12, nSlow = 26, nSig = 9) * 100 #macd,macdSignal
    tatitle <- c(tatitle,c('macd','macdSignal'))
    bbands <- BBands(HLC) #Lower,Middle,Upper,ptcB
    tatitle <- c(tatitle,c('Lower','Middle','Upper','ptcB'))
    atr <- ATR(HLC)
    tatitle <- c(tatitle,names(atr))
    cAD <- chaikinAD(HLC,V)
    tatitle <- c(tatitle,c('cAD'))
    cV <- chaikinVolatility(HL)
    tatitle <- c(tatitle,c('cV'))
    tdi <- TDI(CL)
    tatitle <- c(tatitle,names(tdi))
    adx <- ADX(HLC)
    tatitle <- c(tatitle,names(adx))
    mfi <- MFI(HLC,V)
    tatitle <- c(tatitle,names(mfi))
    obv <- OBV(CL,V)
    tatitle <- c(tatitle,names(obv))
    sar <- SAR(HL)
    tatitle <- c(tatitle,c('sar'))
    dvi <- DVI(CL)
    tatitle <- c(tatitle,c('dviMag','dviStr','dvi'))
    sma10 <-SMA(CL,n = 10)
    tatitle <- c(tatitle,c('sma10'))
    sma20 <-SMA(CL,n = 20)
    tatitle <- c(tatitle,c('sma20'))
    sma50 <-SMA(CL,n = 50)
    tatitle <- c(tatitle,c('sma50'))
    sma100 <-SMA(CL,n = 100)
    tatitle <- c(tatitle,c('sma100'))
    sma150 <-SMA(CL,n = 150)
    tatitle <- c(tatitle,c('sma150'))
    sma250 <-SMA(CL,n = 250)
    tatitle <- c(tatitle,c('sma250'))
    roc5 <-SMA(change,n = 5)
    tatitle <- c(tatitle,c('roc5'))
    roc10 <-SMA(change,n = 10)
    tatitle <- c(tatitle,c('roc10'))
    roc20 <-SMA(change,n = 20)
    tatitle <- c(tatitle,c('roc20'))
    roc50 <-SMA(change,n = 50)
    tatitle <- c(tatitle,c('roc50'))
    roc100 <-SMA(change,n = 100)
    tatitle <- c(tatitle,c('roc100'))
    roc150 <-SMA(change,n = 150)
    tatitle <- c(tatitle,c('roc150'))
    roc250 <-SMA(change,n = 250)
    tatitle <- c(tatitle,c('roc250'))
    data <- cbind(data,change,rsi,sto,smi,macd,bbands,atr,cAD,cV,tdi,adx,mfi,obv,sar,dvi,sma10,sma20,sma50,sma100,sma150,sma250,roc5,roc10,roc20,roc50,roc100,roc150,roc250)
    colnames(data) <- tatitle
    data <- na.omit(data)
    df <- data.frame(date=index(data),coredata=(data))
    print(df)
  } else {
    print('error')
  }
}
