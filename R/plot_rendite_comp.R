#' Plot Returns of different portfolios
#'
#' Plot one portfolio-development in comparison to two other ones
#'
#' @param returns_safe A xts with several columns consisting of data
#' @param portf_safe A vector defining the portfolio weights
#' @param returns_medium A xts with several columns consisting of data
#' @param portf_medium A vector defining the portfolio weights
#' @param returns_risky A xts with several columns consisting of data
#' @param portf_risky A vector defining the portfolio weights
#' @param date_margin A string specifying the start- and enddates
#' @param legend_pos where to set the legend
#'
#' @author Philipp Giese
#' @return A plot with development of tree portfolios
#' @export
#' @examples
#' returns_safe<-calculate_mutliple_cc("CCCAGG",c("BTC","LTC","DASH"),"USD",7,T)
#' portf_safe<-c(0.3,0.4,0.3)
#' returns_medium<-calculate_mutliple_cc("CCCAGG",c("ADA","EOS","ZRX"),"USD",7,T)
#' portf_medium<-c(0.2,0.4,0.4)
#' returns_risky<-calculate_mutliple_cc("CCCAGG",c("PAY","BNB","ZCL"),"USD",7,T)
#' portf_risky<-c(0.2,0.4,0.4)
#' plot_rendite_comp(returns_safe,portf_safe,returns_medium,portf_medium,returns_risky,portf_risky,"20180915/","bottomleft")

plot_rendite_comp <-
function(returns_safe,portf_safe,returns_medium,portf_medium,returns_risky,portf_risky,date_margin,legend_pos){
  plot(100*merge(cumsum(btcecho::get_portf_dev(returns_safe,portf_safe,date_margin))-as.numeric(btcecho::get_portf_dev(returns_safe,portf_safe,date_margin)[1]),
                 cumsum(btcecho::get_portf_dev(returns_medium,portf_medium,date_margin))-as.numeric(btcecho::get_portf_dev(returns_medium,portf_medium,date_margin)[1]),
                 cumsum(btcecho::get_portf_dev(returns_risky,portf_risky,date_margin))-as.numeric(btcecho::get_portf_dev(returns_risky,portf_risky,date_margin)[1])),
       lwd=c(3,3,3), main="Tägliche Rendite in Prozent",
       yaxis.right = F, col=c("dark green","orange","red"))
  addLegend(legend_pos, on=1, legend.names = c("Konservativ","Medium","Risiko"),
            lty=1, lwd=c(3,3,3),col=c("dark green","orange","red"),bty="o")

}
