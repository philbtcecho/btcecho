#' Read price, market cap or volume from list of currency data
#'
#' To get the price, market cap or volume für one specific crypto-
#' curreny is easy, however, applying this selection to a list of
#' xts-triples is more demanding.
#'
#' @param portf a list of cryptocurrencies
#' @param returns data selection. 1 for price, 2 for market cap and
#' 3 for volume
#'
#' @author Philipp Giese
#' @return A time series with portfolio development
#' @examples
#' data<-sapply(c("bitcoin","litecoin"),FUN=function(X) get_all_data_cg(X))
#' get_sel_data_matrix_cg(data,1)

get_complete_returns <-
function(portf,returns){
  dates<-c(index(portf), Sys.Date())
  dates_complete<-seq(as.Date(index(portf)[1]), Sys.Date(),1)
  rendite_safe<-xts::xts(rep(0,length(dates_complete)),order.by = dates_complete)
  for(x in 2:length(dates)){
    if(x==length(dates))
       dates_end<-dates[x]
    else
      dates_end<-dates[x]-1
    date_range<-paste(dates[x-1],"/",dates_end,sep="")
    rendite_safe[date_range,]<-get_portf_dev(returns,
                                             as.numeric(zoo::coredata(portf)[1,]),date_range)
  }
  return(rendite_safe)
}
