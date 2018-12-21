#' Plot Returns of different portfolios
#'
#' Plot one portfolio-development in comparison to two other ones
#'
#' @param date_start A xts with several columns consisting of data
#' @param returns_safe A vector defining the portfolio weights
#' @param portf A xts with several columns consisting of data
#' @param rendite A vector defining the portfolio weights
#' @param label A xts with several columns consisting of data
#' @param thick1 thickness of normal lines
#' @param thick2 thickness of portfolio
#'
#' @author Philipp Giese
#' @return A plot with development of tree portfolios
#' @export
#' @examples
#' returns_safe<-calculate_mutliple_cc("CCCAGG",c("BTC","LTC","DASH"),"USD",7,T)
#' portf_safe<-c(0.3,0.4,0.3)

plot_rendite_artikel <-
  function(date_start,returns_safe,portf,rendite,label, thick1=2, thick2=4){
    dates<-c(zoo::index(portf), Sys.Date())
    for (x in seq(2,length(dates),1)) {
      test<-merge(returns_safe[paste(dates[1],"/",dates[x]-1,sep=""),c("BTC","ETH","XRP")],
                  rendite[paste(dates[1],"/",dates[x]-1,sep="")])}
    for(i in 1:ncol(test)){
      test[date_start,i]<-cumprod(1+test[date_start,i])
      test[date_start,i]<-test[date_start,i]-as.numeric(test[gsub("/","",date_start),i])
    }

    plot(100*(test[date_start]), lwd=c(thick1,thick1,thick1,thick2), main=label, col=c("black","red","dark green","dark blue"))
  }

