#' Get cryptocurrencies from cryptocompare
#'
#' Get a vector with the cryptocurrency-IDs
#' from cryptocompare
#'
#' @author Philipp Giese
#' @return List of coins on cryptocompare
#' @export
#' @examples
#' get_coins_cc()

get_coins_cc <-
function(){
  coins<-jsonlite::fromJSON("https://www.cryptocompare.com/api/data/coinlist/")
  coins_final<-names(coins$Data)
  return(coins_final)
}
