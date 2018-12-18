#' Get thermocap for bitcoin
#'
#' Thermocap is an alternative metric for evaluating cryptocurrencies.
#' Unlike market cap thermocap is a sum over the prices when the coins
#' are generated.
#'
#' @author Philipp Giese
#' @return A time series with portfolio development
#' @examples
#' thermocap()


thermocap <-
function(){
  emission<-jsonlite::fromJSON("https://api.blockchain.info/charts/total-bitcoins?timespan=all&format=json")$values
  emission<-xts::xts(emission$y, order.by = as.Date(as.POSIXct(emission$x, origin="1970-01-01")))
  emission<-diff(emission)
  price<-btcecho::calculate_switch_cc("CCCAGG","BTC","USD",2000,F)$close
  cumsum(emission*price)/10^9
}
