#' Plot Profits
#'
#' Plot the portfolio-development in comparison to BTC, ETH and
#' XRP within the examined timeframe
#'
#' @param returns A xts with several columns consisting of data
#' @param portfolio A vector defining the portfolio weights
#' @param date_margin A string specifying the start- and enddates
#' @param label How to define the examined portfolio within the legend
#' @param legend_pos where to set the legend
#'
#' @author Philipp Giese
#' @return A xts with development of portfolio
#' @export
#' @examples
#' data<-calculate_mutliple_cc("CCCAGG",c("BTC","LTC","DASH"),"USD",7,T)
#' portf<-c(0.3,0.4,0.3)
#' get_portf_dev(data,portf,"201810/")

plot_rendite <-
function(returns,portfolio,returns_safe,date_margin,label,legend_pos){
  perform_safe<-data.frame(as.character(colnames(returns_safe[,c(1,6,5)])),
                           sapply(c(1,6,5),FUN=function(X) mean(returns_safe[date_margin,X]*100)),
                           sapply(c(1,6,5),FUN=function(X) sd(returns_safe[date_margin,X]*100)))
  names(perform_safe)<-c("Asset-Name","Rendite","Risiko")
  print(rbind(perform_safe,c("Fuck off",mean(btcecho::get_portf_dev(returns,portfolio,date_margin))*100,
                             sd(btcecho::get_portf_dev(returns,portfolio,date_margin))*100)))
   plot(100*merge(cumsum(btcecho::get_portf_dev(returns,portfolio,date_margin))-as.numeric(btcecho::get_portf_dev(returns,portfolio,date_margin)[1]),
                 cumsum(returns_safe[date_margin,"BTC"])-as.numeric(returns_safe[date_margin,"BTC"])[1],
                 cumsum(returns_safe[date_margin,"ETH"])-as.numeric(returns_safe[date_margin,"ETH"])[1],
                 cumsum(returns_safe[date_margin,"XRP"])-as.numeric(returns_safe[date_margin,"XRP"])[1]),
       lwd=c(3,1.5,1.5,1.5), main="Tägliche Rendite in Prozent",
       yaxis.right = F, col=c("dark blue","red","blue","green4"))
  return(addLegend(legend_pos, on=1, legend.names = c(label,"Bitcoin","Ethereum","XRP"),
            lty=1, lwd=c(3,1.5,1.5,1.5),col=c("dark blue","red","blue","green4"),bty="o"))
}
