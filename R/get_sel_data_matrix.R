#' Read price, market cap or volume from matrix of currency data
#'
#' To get the price, market cap or volume für one specific crypto-
#' curreny is easy, however, applying this selection to a list of
#' xts-triples is more demanding.
#'
#' @param data a matrix of cryptocurrencies
#' @param sel data selection. 1 for price, 2 for market cap and
#' 3 for volume
#'
#' @author Philipp Giese
#' @return A time series with portfolio development
#' @export
#' @examples
#' data<-sapply(c("bitcoin","litecoin"),FUN=function(X) get_all_data_cg(X))
#' get_sel_data_matrix_cg(data,1)

get_sel_data_matrix_cg <-
function(data,sel){
  data<-data[sel,]
  data_final<-xts::xts(data[1][[1]][,2],
                  order.by = as.Date(as.POSIXct(data[1][[1]][,1]/1000, origin = "1970-01-01")))
  for (x in seq(2,length(data),1)) {
    data_temp<-xts::xts(data[x][[1]][,2],
                   order.by = as.Date(as.POSIXct(data[x][[1]][,1]/1000, origin = "1970-01-01")))
    data_final<-merge(data_final,data_temp,join="left")
  }
  #zeros<-sapply(seq(1,length(data),1),FUN=function(X) length(names(data[[X]])))
  #names(data_final)<-names(data[1,])[zeros>0]
  names(data_final)<-names(data)
  data_final
}
