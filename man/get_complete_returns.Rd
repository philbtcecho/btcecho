% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_complete_returns.R
\name{get_complete_returns}
\alias{get_complete_returns}
\title{Read price, market cap or volume from list of currency data}
\usage{
get_complete_returns(portf, returns)
}
\arguments{
\item{portf}{a list of cryptocurrencies}

\item{returns}{data selection. 1 for price, 2 for market cap and
3 for volume}
}
\value{
A time series with portfolio development
}
\description{
To get the price, market cap or volume für one specific crypto-
curreny is easy, however, applying this selection to a list of
xts-triples is more demanding.
}
\examples{
data<-sapply(c("bitcoin","litecoin"),FUN=function(X) get_all_data_cg(X))
get_sel_data_matrix_cg(data,1)
}
\author{
Philipp Giese
}
