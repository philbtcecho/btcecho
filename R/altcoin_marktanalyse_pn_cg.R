#' Show details about last week for premium newsletter
#'
#' Show development for cryptocurrencies with focus on the top 10.
#'
#' @author Philipp Giese
#' @return A list with means, the winners, the losers, details about the top 10
#' and the differences regarding market cap.
#' @export
#' @examples
#' altcoin_marktanalyse_pn_cg()

altcoin_marktanalyse_pn_cg <-
function(number = 5, days=7){
    global<-jsonlite::read_json("https://api.coingecko.com/api/v3/global",simplifyVector = T)
    test_all<-jsonlite::read_json(paste("https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&order=market_cap_desc&per_page=100&page=1&sparkline=false&price_change_percentage=",days,"d",sep=""), simplifyVector = T)
    data_100<-test_all[c(3,5,22)]
    data_100$current_price<-round(as.numeric(data_100$current_price),digits = 2)
    data_10<-data_100[1:number,]
    data_10$price_change_percentage_7d_in_currency<-round(as.numeric(data_10$price_change_percentage_7d_in_currency),digits = 1)
    data_100<-data_100[order(as.numeric(data_100$price_change_percentage_7d_in_currency),decreasing = T),]
    data_10_short<-data_10
    names(data_10_short)<-c("Name","price","change_7d")
    test<-list(head(data_100),
               tail(data_100),
               data_10_short)
    names(test)<-c("best100","worst100","top10")
    return(test)
  }
