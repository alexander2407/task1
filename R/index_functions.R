#' Get Dataframe of Symbols and Names
#'
#' This function allows you to get a data frame of components (symbol, company, last_price, change, change_pr) of an index.
#' The possible indices are ATX, SMI or DAX. Every other text as input leads to an error.
#' @param index (string) index that you want to receive the components of (default is atx)
#' @keywords index, stock, symbol, yahoo
#' @examples getSymbolsOfIndex("atx")
#'
#' @import rvest
#' @export
getSymbolsOfIndex <- function(index = "atx") {
  index <- switch (tolower(index),
    "atx" = "ATX",
    "smi" = "SSMI",
    "dax" = "GDAXI",
    "error"
  )

  if(index=="error"){
    stop("Index has to be: ATX, SMI or DAX")
  }

  pageUrl <- paste("https://finance.yahoo.com/quote/%5E",toupper(index),"/components?p=%5E",toupper(index),sep="")
  index_html <- read_html(pageUrl)

  tables <- html_table(index_html, fill = TRUE)
  table_indexes <- tables[[1]]
  column_names <-  c("symbol", "company", "last_price", "change", "change_pr", "volume")
  colnames(table_indexes) <-  column_names
  table_indexes$volume <- as.numeric(gsub(",", "", table_indexes$volume))

  return(table_indexes[-6]) # to avoid name conflict with volume from tidyquant, that would lead to a nested data frame
}
