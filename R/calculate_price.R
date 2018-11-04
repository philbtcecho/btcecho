#' Get price from cryptocompare
#'
#' Read the OHLC-values for a distinct cryptocurrency_from in units
#' of cryptocurrency_to at the exchange for the last days
#'
#' @param exchange A string defining the exchange
#' @param currency_from A string defining the analyzed currency
#' @param currency_to A string defining the base currency
#' @param days An integer defining past x days analyzed
#'
#' @author Philipp Giese
#' @return A time series with ohlc-values and volume
#' @examples
#' calculate_price("CCCAGG","BTC","USD",7)

calculate_price <-
function(exchange,currency_from,currency_to,days){
  try(Daten<-jsonlite::fromJSON(
    paste("https://min-api.cryptocompare.com/data/histoday?fsym="
          ,currency_from,"&tsym=",currency_to,"&limit=",days,"&aggregate=1&e="
          ,exchange,sep="")))
  try(return(xts::xts(Daten$Data[2:7],
    order.by = as.Date(as.POSIXct(Daten$Data[[1]],origin="1970-01-01")))))
}

