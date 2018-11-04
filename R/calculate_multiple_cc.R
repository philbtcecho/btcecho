#' Get prices or gains for several Cryptocurrencies
#' from cryptocompare
#'
#' Read the OHLC-Values + Volume or the gains for a list of crypto-
#' currencies.
#'
#' @param exchange A string defining the exchange
#' @param currencies_from A vector of strings defining the
#' analyzed currencies
#' @param currency_to A string defining the base currency
#' @param days An integer defining past x days analyzed
#' @param gains boolean, T for gains, F for OHLC-values
#'
#' @author Philipp Giese
#' @return A time series with gains (when gains = T)
#' or ohlc-values and volume (when gains = F)
#' @export
#' @examples
#' calculate_multiple_cc("CCCAGG",c("BTC","LTC","test","CRAIG"),"USD",7,T)


calculate_multiple_cc <-
  function(exchange="CCCAGG", currencies_from, currency_to="USD", days, gains){
    try(data_final <- calculate_switch_cc(exchange, currencies_from[1],currency_to,days,gains)$close)
    if(length(currencies_from)>1){
      for (x in seq(2,length(currencies_from),1)) {
        try(data_temp<-tryCatch(
          calculate_switch_cc(exchange, currencies_from[x],currency_to,days,gains)$close,
          error=function(cond){
            temp<-xts::xts(rep(-1,days+1),order.by = zoo::index(calculate_switch_cc("CCCAGG", "BTC","USD",days,T)))
            names(temp)<-"close"
            temp
          }))
        try(data_final<-merge(data_final,data_temp,join="left"))
      }
    }
    try(names(data_final)<-currencies_from)
    try(data_final)
  }
