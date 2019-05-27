#' Show details about last week
#'
#' Show development for cryptocurrencies with focus on the top 10.
#'
#' @author Philipp Giese
#' @return A list with means, the winners, the losers, details about the top 10
#' and the differences regarding market cap.
#' @export
#' @examples
#' altcoin_marktanalyse_cg()

altcoin_marktanalyse_cg <-
function(number = 10, makef = T){
    global<-jsonlite::read_json("https://api.coingecko.com/api/v3/global",simplifyVector = T)
    test_all<-jsonlite::read_json(paste("https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&order=market_cap_desc&per_page=100&page=1&sparkline=false&price_change_percentage=7d",sep=""), simplifyVector = T)
    data_100<-test_all[c(3,5,6,22)]
    data_100$current_price<-round(as.numeric(data_100$current_price),digits = 2)
    data_100$market_cap<-round(as.numeric(data_100$market_cap)/10^9, digits = 2)
    data_10<-data_100[1:number,]
    data_10$price_change_percentage_7d_in_currency<-round(as.numeric(data_10$price_change_percentage_7d_in_currency),digits = 1)
    differences<-data.frame(data_100$name[1:(number+1)],data_100$name[2:(number+2)],(as.numeric(data_100$market_cap[1:(number+1)])/as.numeric(data_100$market_cap[2:(number+2)])-1)*100)
    names(differences)<-c("currh","currl","diff")
    data_100<-data_100[order(as.numeric(data_100$price_change_percentage_7d_in_currency),decreasing = T),]
    data_10_nice<-data_10
    data_10_short<-data_10
    names(data_10_short)<-c("Name","price","mcap","change_7d")
    names(data_10_nice)<-c("Name","Kurs in US-Dollar","Marktkapitalisierung in Mrd. US-Dollar","Wöchentliche Kursänderung in Prozent")
    test<-list(round(global$data$total_market_cap$usd/10^9),
               round(global$data$market_cap_percentage$btc),
               mean(as.numeric(data_10$price_change_percentage_7d_in_currency)),
               mean(as.numeric(data_100$price_change_percentage_7d_in_currency),na.rm = T),
               nrow(data_100[as.numeric(data_100$price_change_percentage_7d_in_currency)>0,]),
               head(data_100),
               tail(data_100),
               nrow(data_100[as.numeric(data_100$price_change_percentage_7d_in_currency)<as.numeric(data_100$price_change_percentage_7d_in_currency[data_100$name=="Bitcoin"]),]),
               data_10_short,
               differences)
    names(test)<-c("MCap_total","BTC_Share","MW10","MW100","pos100","best100","worst100","betterbitcoin100","top10","differences")
    if(makef)
      write.csv(data_10_nice,file = "Topten.csv",row.names = F,col.names = F,sep = ";",dec=",")
    return(test)
  }
