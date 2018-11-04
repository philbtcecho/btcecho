#' Read portfolio development from Excel file
#'
#' Read the portfolio development from an excel file. The excel sheet
#' should have the following structure:
#' <empty> <Asset_1> <Asset_2>...
#' <date> <weight_Asset_1> <weight_Asset_2>...
#' ...
#'
#' @param file the path to the xlsx file
#' @param sheet the sheet where the data is to be found
#'
#' @author Philipp Giese
#' @return A time series with portfolio development
#' @examples
#' calculate_gains(calculate_price("CCCAGG","BTC","USD",7))
#'
read_portf_excel <-
function(file,sheet){
  portf<-readxl::read_xlsx(file,sheet = sheet)
  xts::xts(portf[,2:ncol(portf)],order.by = as.Date(portf[,1][[1]]))
}
