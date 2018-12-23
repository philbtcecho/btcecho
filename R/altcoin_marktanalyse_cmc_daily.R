#' Generate Coin Report about
#'
#' Show development for cryptocurrencies with focus on the top 10.
#'
#' @author Philipp Giese
#' @return A list with means, the winners, the losers, details about the top 10
#' and the differences regarding market cap.
#' @export
#' @examples
#' altcoin_marktanalyse_cmc_daily(2,path)

altcoin_marktanalyse_cmc_daily <-
  function(number,file_path){
    texte<-read.csv(file_path,header = T, sep=";")
    global<-jsonlite::read_json("https://api.coinmarketcap.com/v1/global/?convert=usd",simplifyVector = T)[1:3]
    texte_header<-gsub("BBB",round(global[[3]]),
                       gsub("CCC",round(global[[2]]/10^9),
                            gsub("AAA",round(global[[1]]/10^9),
                                 sample(as.character(texte$description[texte$sentiment=="header"]),1,replace = T))))
    test_all<-jsonlite::read_json(paste("https://api.coinmarketcap.com/v1/ticker/?convert=usd&limit=",number,sep=""), simplifyVector = T)
    data_all<-test_all[c(2,4,5,7,8,13)]
    data_all$market_cap_usd<-round(as.numeric(data_all$market_cap_usd)/10^9, digits=2)
    data_all$`24h_volume_usd`<-round(as.numeric(data_all$`24h_volume_usd`)/10^6, digits=2)
    data_all$price_usd<-round(as.numeric(data_all$price_usd),digits = 2)
    texte_final<-rep(number+1)
    for (i in 1:number){
      if (as.numeric(data_all$percent_change_24h[i])>2)
        if (as.numeric(data_all$percent_change_24h[i])>7)
          sentiment<-"sehr bullish"
        else
          sentiment<-"bullish"
        else
          if (as.numeric(data_all$percent_change_24h[i])< -2)
            if (as.numeric(data_all$percent_change_24h[i])< -7)
              sentiment<- "sehr bearish"
            else
              sentiment<-"bearish"
            else
              sentiment<-"neutral"
            texte_final[i+1]<-paste(btcecho:::get_text(data_all[i,],sentiment,texte),
                                    gsub("UU",data_all$price_usd[i],
                                         sample(as.character(texte$description[texte$sentiment=="price"]),1,
                                                replace = T)))
    }
    texte_final[1]<-texte_header
    paste(texte_final,collapse = "\n\n",sep=" ")
  }
