#' Get Supports and Resistances
#'
#' Generates a density plot wit a chosen width of the close-values of a ohlc-xts and lists the maxima
#'
#' @param ohlc an xts with the close, high, low, open and volume-Vals
#' @param max_past An integer defining past x days analyzed
#' @param width binning width
#'
#' @author Philipp Giese
#' @return a density plot with the maxima and a vector with all maxima
#' @export
#' @examples
#' get_sup_resi(btcecho:::calculate_price("CCCAGG","BNB","USD",2000),"20180301/",0.1)

get_sup_resi <-
function(ohlc,max_past,width){
    start<-paste(gsub("-","",Sys.Date()-max_past),"/",sep="")
    density_vals<-data.frame(density(ohlc$close[start],width=width)$x,density(ohlc$close[start],width=width)$y)
    names(density_vals)<-c("x","y")
    t_y<-ts(density_vals$y)
    test<-pastecs::turnpoints(t_y)
    punkte<-test$tppos
    sup_resi<-density_vals$x[punkte[seq(1,length(punkte),2)]]
    plot(density(ohlc$close[start],width=width))
    points(sup_resi,density_vals$y[punkte[seq(1,length(punkte),2)]],col=ifelse(sup_resi<as.numeric(xts::last(ohlc$close[start])),"red","dark green"))
    return(sup_resi)
}
