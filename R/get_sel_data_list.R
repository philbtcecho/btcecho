#' Read price, market cap or volume from list of currency data
#'
#' To get the price, market cap or volume für one specific crypto-
#' curreny is easy, however, applying this selection to a list of
#' xts-triples is more demanding.
#'
#' @param data a list of cryptocurrencies
#' @param sel data selection. 1 for price, 2 for market cap and
#' 3 for volume
#'
#' @author Philipp Giese
#' @return A time series with portfolio development
#' @export
#' @examples
#' data<-sapply(c("bitcoin","litecoin"),FUN=function(X) get_all_data_cg(X))
#' get_sel_data_matrix_cg(data,1)

get_sel_data_list_cg <-
function(data,sel){
    data_final<-xts(data[[1]][[sel]][,2],
                    order.by = as.Date(as.POSIXct(data[[1]][[sel]][,1]/1000, origin = "1970-01-01")))
    for (x in seq(2,length(data),1)) {
        if(length(names(data[[x]]))>0){
            data_temp<-xts(data[[x]][[sel]][,2],
                           order.by = as.Date(as.POSIXct(data[[x]][[sel]][,1]/1000, origin = "1970-01-01")))
            data_final<-merge(data_final,data_temp,join="left")
        }
    }
    zeros<-sapply(seq(1,length(data),1),FUN=function(X) length(names(data[[X]])))
    names(data_final)<-names(data)[zeros>0]
    data_final
}
