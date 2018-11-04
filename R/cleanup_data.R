#' Clean Data from outliers
#'
#' Clean a time series from outliers. The definition of an outlier
#' is a value who is outside an interval mean(data)+/-k_value*sd(data)
#' with data being a column of a time series and k_value an integer.
#'
#' @param returns_clean A xts with several columns consisting of data
#' @param k_value A number defining which outliers should be neglected
#'
#' @author Philipp Giese
#' @return A xts with less outliers
#' @export
#' @examples
#' data<-calculate_mutliple_cc("CCCAGG",c("BTC","LTC","DASH"),"USD",7,T)
#' cleanup_data(data,5)


cleanup_data <-
function(returns_clean,k_value){
  #returns_prices_clean<-returns_prices
  outliers<-sapply(1:ncol(returns_clean),FUN=function(X) which(as.vector(returns_clean[,X]) %in% as.vector(returns_clean[as.vector(returns_clean[,X])>=mean(as.vector(returns_clean[,X]),na.rm = T)+k_value*sd(as.vector(returns_clean[,X]),na.rm = T) | as.vector(returns_clean[,X])<=mean(as.vector(returns_clean[,X]),na.rm = T)-k_value*sd(as.vector(returns_clean[,X]),na.rm = T),X])))
  for(X in 1:ncol(returns_clean))
  {
    if(length(outliers[[X]])==0)
      next
    for(Y in 1:length(outliers[[X]]))
      returns_clean[outliers[[X]][Y],X]<-ifelse(outliers[[X]][Y]==1,returns_clean[outliers[[X]][Y]+1,X],
                                                ifelse(outliers[[X]][Y]==nrow(returns_clean),returns_clean[outliers[[X]][Y]-1,X],(returns_clean[outliers[[X]][Y]+1,X]+as.numeric(returns_clean[outliers[[X]][Y]-1,X]))/2))
  }
        return(returns_clean)
}
