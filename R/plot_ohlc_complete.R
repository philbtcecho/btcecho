#' Plot OHLC complete
#'
#' Generate a nice OHLC/candlestick plot with different moving averages
#' plus a MACD, a RSI and an Aroon-indicator
#' To do: yaxis ticks and format of indicators
#'
#' @param ohlc an xts with the close, high, low, open and volume-Vals
#' @param ymin minimal value on y-axis
#' @param ymax maximal value on y-axis
#' @param max_past An integer defining past x days analyzed
#'
#' @author Philipp Giese
#' @return A nifty plot with MA140, MA980 and MA1400 and other indicators
#' @export
#' @examples
#' plot_ohlc_complete(btcecho:::calculate_price("CCCAGG","BNB","USD",2000),0,20,365)

plot_ohlc_complete <-
function(ohlc,ymin,ymax,max_past){
    mytheme<-quantmod::chart_theme()
    mytheme$col$up.col<-"dark green"
    mytheme$col$grid<-"#b3b3b3"
    mytheme$col$grid2<-"#666666"
    mytheme$format.labels<-F
    ohlc<-ohlc[,c(4,2,3,1)]
    end_date<-paste(gsub("-","",Sys.Date()-max_past),"/",sep="")
    temp<-NULL
    temp<-quantmod::chart_Series(ohlc,theme = mytheme,name="",subset=end_date)
    mainylim<-temp$get_ylim()
    mainylim[[2]]<-structure(c(ymin,ymax),fixed=T)
    temp$set_ylim(mainylim)
    temp <- quantmod::add_TA(TTR::SMA(ohlc[,4],1400),lwd=3,on=1,col="red",legend = NULL)
    temp <- quantmod::add_TA(TTR::SMA(ohlc[,4],980),lwd=3,on=1,col="blue",legend = NULL)
    temp <- quantmod::add_TA(TTR::SMA(ohlc[,4],140),lwd=3,on=1,col="dark green",legend = NULL)
    temp
    quantmod::add_MACD(histogram=F)
    quantmod::add_RSI()
    quantmod::add_TA(TTR::aroon(ohlc[,2:3],14)[,1], col = "orange")
    quantmod::add_TA(TTR::aroon(ohlc[,2:3],14)[,2],on=4, col = "light blue")

}
