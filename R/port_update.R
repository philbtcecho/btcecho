#' Plot Returns of different portfolios
#'
#' Plot one portfolio-development in comparison to two other ones
#'
#' @param date_margin A string specifying the start- and enddates
#' @param returns A xts with several columns consisting of data
#' @param offset A vector defining the portfolio weights
#' @param portfolio A vector defining the portfolio weights
#'
#' @author Philipp Giese
#' @return A xts with development of portfolio
#' @export
#' @examples
#' returns_safe<-calculate_mutliple_cc("CCCAGG",c("BTC","LTC","DASH"),"USD",7,T)
#' portf_safe<-c(0.3,0.4,0.3)
#' returns_medium<-calculate_mutliple_cc("CCCAGG",c("ADA","EOS","ZRX"),"USD",7,T)
#' portf_medium<-c(0.2,0.4,0.4)
#' returns_risky<-calculate_mutliple_cc("CCCAGG",c("PAY","BNB","ZCL"),"USD",7,T)
#' portf_risky<-c(0.2,0.4,0.4)
#' plot_rendite_comp(returns_safe,portf_safe,returns_medium,portf_medium,returns_risky,portf_risky,"20180915/","bottomleft")

port_update <-
function(date_margin,returns,offset,portfolio){
  min_portf<-portfolio/(offset+1)
  PerformanceAnalytics::portfolio.optim(returns[date_margin,],reslow = min_portf)
}
