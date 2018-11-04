#' Show Portfolio development
#'
#' Show the development of a weighted portfolio
#'
#' @param returns A xts with several columns consisting of data
#' @param portfolio A vector defining the portfolio weights
#' @param date_margins A string specifying the start- and enddates
#'
#' @author Philipp Giese
#' @return A xts with development of portfolio
#' @export
#' @examples
#' data<-calculate_mutliple_cc("CCCAGG",c("BTC","LTC","DASH"),"USD",7,T)
#' portf<-c(0.3,0.4,0.3)
#' get_portf_dev(data,portf,"201810/")

get_portf_dev <-
function(returns,portfolio,date_margins)
  xts::xts(rowSums(sapply(seq(1,length(portfolio),1),
                          FUN=function(X) zoo::coredata(returns[date_margins,X])*portfolio[X])),
           order.by = zoo::index(returns[date_margins,1]))
