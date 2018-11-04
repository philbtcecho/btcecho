#' Get price or gains from cryptocompare
#'
#' Read the OHLC-values or gains for a distinct cryptocurrency_from
#'in units of cryptocurrency_to at the exchange for the last days
#'
#' @param exchange A string defining the exchange
#' @param currency_from A string defining the analyzed currency
#' @param currency_to A string defining the base currency
#' @param days An integer defining past x days analyzed
#' @param gains boolean, T for gains, F for OHLC-values
#'
#' @author Philipp Giese
#' @return A time series with ohlc-values and volume
#' @export
#' @examples
#' calculate_switch_cc("CCCAGG","BTC","USD",7,T)

calculate_switch_cc <-
function(exchange, currency_from, currency_to, days, gains){
  try(daten<-calculate_price(exchange, currency_from, currency_to, days))
  try(if(gains)
    calculate_gains(daten)
  else
    daten)
}

