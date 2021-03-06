% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_all_data_cg.R
\name{get_all_data_cg}
\alias{get_all_data_cg}
\title{Get all available historical data for a coin}
\usage{
get_all_data_cg(coin, coin_basis, days)
}
\arguments{
\item{coin}{coin name eg "bitcoin"}

\item{coin_basis}{base currency eg "usd"}

\item{days}{number of days, eg number or "max"}
}
\value{
A list of time series with price, market cap and volume
}
\description{
Get daily price, market cap and volume for a specific crypto-
currency from coingecko
}
\examples{
get_all_data_cg("bitcoin","usd","max"))
}
\author{
Philipp Giese
}
