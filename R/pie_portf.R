#' Pie Plot of Portfolio
#'
#' Make a pieplot of the current portfolio
#'
#' @param returns A xts with several columns consisting of data
#' @param portfolio A vector defining the portfolio weights
#'
#' @author Philipp Giese
#' @return A xts with development of portfolio
#' @export
#' @examples
#' data<-calculate_mutliple_cc("CCCAGG",c("BTC","LTC","DASH"),"USD",7,T)
#' portf<-c(0.3,0.4,0.3)
#' get_portf_dev(data,portf,"201810/")

pie_portf <-
function(portfolio,returns){
  pie(portfolio[portfolio>0.01],labels = colnames(returns)[portfolio>0.01],col=rainbow(length(colnames(returns)[portfolio>0.01])))
}
