#' Show details about last week
#'
#' Show development for cryptocurrencies with focus on the top 10.
#'
#' @author Philipp Giese
#' @return A list with means, the winners, the losers, details about the top 10
#' and the differences regarding market cap.
#' @export
#' @examples
#' altcoin_marktanalyse_cmc()

altcoin_marktanalyse_cmc <-
function(circulating = F, number = 10, nmax = 3000, makef = T){
  global<-jsonlite::read_json("https://api.coinmarketcap.com/v1/global/?convert=usd",simplifyVector = T)
  test_all<-jsonlite::read_json(paste("https://api.coinmarketcap.com/v1/ticker/?convert=usd&limit=",nmax,sep=""), simplifyVector = T)
  data_all<-test_all[c(2,3,5,8,10,14)]
  if(circulating){
    data_all$market_cap_usd<-round(as.numeric(data_all$market_cap_usd)/10^9,digits = 2)
  }else
    data_all$market_cap_usd<-round(as.numeric(data_all$total_supply)*as.numeric(data_all$price_usd)/10^9,digits=2)
  data_all<-data_all[c(1,2,3,4,6)]
  data_all$price_usd<-round(as.numeric(data_all$price_usd),digits = 2)
  data_all<-data_all[order(data_all$market_cap_usd,decreasing = T),]
  data_100<-data_all[1:100,]
  data_100<-data_100[order(as.numeric(data_100$percent_change_7d),decreasing = T),]
  data_10<-data_all[1:number,]
  symbols_10<-data_10$symbol
  data_10<-data_10[c(1,3,4,5)]
  differences<-data.frame(data_all$name[1:(number+1)],data_all$name[2:(number+2)],(as.numeric(data_all$market_cap_usd[1:(number+1)])/as.numeric(data_all$market_cap_usd[2:(number+2)])-1)*100)
  names(differences)<-c("currh","currl","diff")
  test<-list(mean(as.numeric(data_10$percent_change_7d)),
             mean(as.numeric(data_all$percent_change_7d), na.rm = T),
             mean(as.numeric(data_100$percent_change_7d),na.rm = T),
             nrow(data_100[as.numeric(data_100$percent_change_7d)>0,]),
             head(data_100),
             tail(data_100),
             nrow(data_100[as.numeric(data_100$percent_change_7d)<as.numeric(data_100$percent_change_7d[data_100$name=="Bitcoin"]),]),
             data_10,
             differences,
             symbols_10)
  names(test)<-c("MW10","MWall","MW100","pos100","best100","worst100","betterbitcoin100","top10","differences","symbols10")
  if(makef)
    write.csv(data_10,file = "Topten.csv",row.names = F,col.names = F,sep = ";",dec=",")
  return(test)
}




