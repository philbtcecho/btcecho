#' Generate Coin Report about Top 10 based on Coingecko
#'
#' Show development for cryptocurrencies with focus on the top 10.
#'
#' @author Philipp Giese
#' @return A list with means, the winners, the losers, details about the top 10
#' and the differences regarding market cap.
#' @export
#' @examples
#' altcoin_marktanalyse_cg_daily(10,path)

altcoin_marktanalyse_cg_daily <-
function(number, file_path){
  texte<-read.csv(file_path,header = T, sep=";")
  global<-jsonlite::read_json("https://api.coingecko.com/api/v3/global",simplifyVector = T)
  texte_header<-gsub("BBB",round(global$data$market_cap_percentage$btc),
                     gsub("CCC",round(global$data$total_volume$usd/10^9),
                          gsub("AAA",round(global$data$total_market_cap$usd/10^9),
                               sample(as.character(texte$description[texte$sentiment=="header"]),1,replace = T))))
  texte_header<-paste(texte_header,"<br>",sep="")
  test_all<-jsonlite::read_json("https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&order=market_cap_desc&per_page=100&page=1&sparkline=false&price_change_percentage=24h", simplifyVector = T)
  winners<-head(test_all[order(as.numeric(test_all$price_change_percentage_24h),decreasing = T),c(1,3,5,12)],n=5)
  winners$current_price<-round(as.numeric(winners$current_price),digits=2)
  winners$price_change_percentage_24h<-round(as.numeric(winners$price_change_percentage_24h),2)
  winners$price_change_percentage_24h<-paste("<span style = \"color:",ifelse(sign(winners$price_change_percentage_24h)==-1,"#fb0000;\">","#00b600;\">"),winners$price_change_percentage_24h,"</span>",sep="")
  losers<-tail(test_all[order(as.numeric(test_all$price_change_percentage_24h),decreasing = T),c(1,3,5,12)],n=5)
  losers$current_price<-round(as.numeric(losers$current_price),digits=2)
  losers$price_change_percentage_24h<-round(as.numeric(losers$price_change_percentage_24h),2)
  losers$price_change_percentage_24h<-paste("<span style = \"color:",ifelse(sign(losers$price_change_percentage_24h)==-1,"#fb0000;\">","#00b600;\">"),losers$price_change_percentage_24h,"</span>",sep="")
  winnertable<-paste("<ul class=\"btce-list--top5\">",paste(sapply(1:nrow(winners), FUN=function(x) paste("<li>",paste("<b><a href='http://www.btc-echo.de/kurs/",winners$id[x],"'>",winners$name[x],"-Kurs</a></b>",sep=""),": ",winners[x,3]," US-Dollar (",winners[x,4]," %)</li>",sep="")), collapse="\n",sep=""),"</ul>",sep="")
  losertable<-paste("<ul class=\"btce-list--flop5\">",paste(sapply(1:nrow(losers), FUN=function(x) paste("<li>",paste("<b><a href='http://www.btc-echo.de/kurs/",losers$id[x],"'>",losers$name[x],"-Kurs</a></b>",sep=""),": ",losers[x,3]," US-Dollar (",losers[x,4]," %)</li>",sep="")), collapse="\n",sep=""),"</ul>",sep="")
  data_all<-test_all[1:number,c(1,3,5,6,8,9,10,12)]
  data_all$names10<-paste(gsub(" ","-",data_all$name),"-Kurs",sep="")
  data_all$market_cap<-round(as.numeric(data_all$market_cap)/10^9, digits=2)
  data_all$total_volume<-round(as.numeric(data_all$total_volume)/10^6, digits=2)
  data_all$current_price<-gsub(" ","",format(round(as.numeric(data_all$current_price),digits = 2),nsmall = 2))
  data_all$links<-sapply(1:number,FUN=function(X) paste("<b><a href='http://www.btc-echo.de/kurs/",data_all$id[X],"'>",paste(data_all$name[X],"-Kurs",sep=""),"</a></b>",sep=""))
  data_all$price_change_percentage_24h<-round(as.numeric(data_all$price_change_percentage_24h),2)
  data_all$satzstruktur[data_all$price_change_percentage_24h >=-2 & data_all$price_change_percentage_24h<=2]<-sample(as.character(texte$description[texte$sentiment=="neutral"]),nrow(data_all[data_all$price_change_percentage_24h >=-2 & data_all$price_change_percentage_24h<=2,]),replace = F)
  data_all$satzstruktur[data_all$price_change_percentage_24h < -2 & data_all$price_change_percentage_24h>=-7]<-sample(as.character(texte$description[texte$sentiment=="bearish"]),nrow(data_all[data_all$price_change_percentage_24h < -2 & data_all$price_change_percentage_24h>=-7,]),replace = F)
  data_all$satzstruktur[data_all$price_change_percentage_24h < -7]<-sample(as.character(texte$description[texte$sentiment=="sehr bearish"]),nrow(data_all[data_all$price_change_percentage_24h < -7,]),replace = F)
  data_all$satzstruktur[data_all$price_change_percentage_24h > 2 & data_all$price_change_percentage_24h<=7]<-sample(as.character(texte$description[texte$sentiment=="bullish"]),nrow(data_all[data_all$price_change_percentage_24h > 2 & data_all$price_change_percentage_24h<= 7,]),replace = F)
  data_all$satzstruktur[data_all$price_change_percentage_24h > 7]<-sample(as.character(texte$description[texte$sentiment=="sehr bullish"]),nrow(data_all[data_all$price_change_percentage_24h > 7,]),replace = F)
  data_all$satzstruktur[data_all$name=="Tether"]<-sample(as.character(texte$description[texte$sentiment == "stableok"]),1,replace = F)
  data_all$satzstruktur[data_all$name=="Tether" & abs(data_all$price_change_percentage_24h)>2]<-sample(as.character(texte$description[texte$sentiment == "stableneutral"]),1,replace = F)
  data_all$satzstruktur[data_all$name=="Tether" & abs(data_all$price_change_percentage_24h)>7]<-sample(as.character(texte$description[texte$sentiment == "stableextreme"]),1,replace = F)
  data_all$final<-sapply(1:number,FUN=function(x) gsub("XX",data_all$names10[x],data_all$satzstruktur[x]))
  data_all$final<-sapply(1:number,FUN=function(x) gsub("YY",abs(data_all$price_change_percentage_24h[x]),data_all$final[x]))
  data_all$final<-sapply(1:number,FUN=function(x) gsub("ZZ",data_all$price_change_percentage_24h[x],data_all$final[x]))
  data_all$imagenr<-64578
  data_all$imagenr[data_all$price_change_percentage_24h>2]<-64582
  data_all$imagenr[data_all$price_change_percentage_24h< -2]<-64581
  data_all$imagelink<-"side.png"
  data_all$imagelink[data_all$price_change_percentage_24h< -2]<- "down.png"
  data_all$imagelink[data_all$price_change_percentage_24h>2]<-"up.png"
  data_all$final<-paste("<h2>",data_all$name," <img class=\"alignnone  wp-image-",data_all$imagenr,"\" src=\"https://www.btc-echo.de/wp-content/uploads/2019/01/",data_all$imagelink,"\" alt=\"\" width=\"15\" height=\"15\" /></h2><br>",data_all$final,sep="")
  data_all$price<-sample(as.character(texte$description[texte$sentiment=="price"]),number, replace = F)
  data_all$price<-sapply(1:number,FUN=function(x) gsub("XX",data_all$name[x],data_all$price[x]))
  data_all$price<-sapply(1:number,FUN=function(x) gsub("UU",data_all$current_price[x],data_all$price[x]))
  data_all$final<-paste(data_all$final," ",data_all$price,"<ul><li>Marktkapitalisierung: ",data_all$market_cap," Milliarden US-Dollar</li><li>Handelsvolumen: ",round(data_all$total_volume,0)," Milionen US-Dollar</li><li>24h-Hoch: ",round(as.numeric(data_all$high_24h),2)," US-Dollar</li><li> 24h-Tief: ",round(as.numeric(data_all$low_24h),2)," US-Dollar</li></ul><br>Zum aktuellen ",data_all$links,"<br>", sep="")
  return(c(paste(data_all$final,collapse = "<br><br>",sep=" "),mean(data_all$price_change_percentage_24h),texte_header, winnertable, losertable))
}
