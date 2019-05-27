#' Get all available historical data for a coin
#'
#' Get daily price, market cap and volume for a specific crypto-
#' currency from coingecko
#'
#' @param coin coin name eg "bitcoin"
#' @param coin_basis base currency eg "usd"
#' @param days number of days, eg number or "max"
#' @param sel selection of Data: 1 for price, 2 for market cap and 3 for volume.
#'
#' @author Philipp Giese
#' @return A xts either of price, market cap and volume
#' @export
#' @examples
#' get_sel_data_cg("bitcoin","usd","max",1)

get_sel_data_cg <-
  function(coin,coin_basis,days,sel){
    data<-jsonlite::read_json(paste("https://api.coingecko.com/api/v3/coins/",coin,"/market_chart?vs_currency=",coin_basis,"&days=",days,sep=""), simplifyVector = T)
    data<-data[[sel]]
    data_final<-xts::xts(data[,2],
                         order.by = as.POSIXct(data[,1]/1000, origin = "1970-01-01"))
    data_final
  }
