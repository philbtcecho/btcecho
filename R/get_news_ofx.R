#' Get news from onchainfx
#'
#' Show a curated list of the top last 50 news by the OnchainFx-Team
#'
#' @param coin optional specification for coin (default empty)
#
#' @author Philipp Giese
#' @return A list with fifty news regarding the specified cryptocurr
#' @export
#' @examples
#' get_news_ofx()

get_news_ofx <-
function(coin=NULL){
  data<-jsonlite::read_json(paste("https://data.messari.io/api/v1/news",if(!is.null(coin)) paste("/",coin,sep=""),sep=""))
  refs<-sapply(1:50, FUN=function(X) data$data[[X]]$references[[1]]$url)
  titles<-sapply(1:50, FUN=function(X) data$data[[X]]$title)
  datetime<-sapply(1:50, FUN=function(X) data$data[[X]]$published_at)
  content<-sapply(1:50, FUN=function(X) data$data[[X]]$content)
  return(cbind(datetime,titles,refs,content))
}
