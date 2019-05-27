#' Get all available historical data for a coin
#'
#' Get daily price, market cap and volume for a specific crypto-
#' currency from coingecko
#'
#' @param coin coin name eg "bitcoin"
#' @param coin_basis base currency eg "usd"
#' @param days number of days, eg number or "max"
#'
#' @author Philipp Giese
#' @return A list of time series with price, market cap and volume
#' @export
#' @examples
#' get_all_data_cg("bitcoin","usd","max"))

get_all_data_cg <-
function(coin,coin_basis,days){
  try(return(jsonlite::read_json(paste("https://api.coingecko.com/api/v3/coins/",coin,"/market_chart?vs_currency=",coin_basis,"&days=",days,sep=""), simplifyVector = T)))
}
