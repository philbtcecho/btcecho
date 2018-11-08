#' Get gains from cryptocompare
#'
#' Calculate the gains of a cryptocurrency with respect to the
#' closing price within a xts of ohlc-values. Referred_to columns
#' must be named "close" and "open".
#'
#' @param data A xts with ohlc-values
#'
#' @author Philipp Giese
#' @return A time series with gains
#' @examples
#' calculate_gains(calculate_price("CCCAGG","BTC","USD",7))

calculate_gains <-
function(data){
    try(return(TTR::ROC(data,na.pad = F,type="discrete")))
  }
