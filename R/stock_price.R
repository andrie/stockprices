#' Get stock prices from yahoo finance.
#' 
#' Use `quantmod` to read ticker data from yahoo finance.
#' 
#' @inherit update_ftse_components
#' @param ticker ticker
#' 
#' @param wait time in seconds to wait between yahoo reads
#' @return tibble with ticker price data
#' @export
#' 
#' @importFrom quantmod getSymbols
#' @importFrom lubridate ymd
#' @importFrom tibble rownames_to_column tibble
#' @importFrom magrittr set_names
scrape_stock_price <- function(ticker, wait = 0.1){
  
  Date <- Adjusted <- . <- NULL

  options("getSymbols.warning4.0" = FALSE)
  options("getSymbols.yahoo.warning" = FALSE)
  
  empty_tibble <- function(){
    tibble(
      "Date" = Sys.Date(), 
      "Open" = NA_real_, 
      "High" = NA_real_, 
      "Low" = NA_real_, 
      "Close" = NA_real_,
      "Volume" = NA, 
      "Adjusted" = NA_real_, 
      Ticker = ticker, 
      "LogReturn" = NA_real_)
  }
  
  log_returns <- function(x){ c(NA, diff(log(x), lag=1)) }
  Sys.sleep(wait)
  z <- tryCatch(
    suppressWarnings(
      getSymbols(ticker, env = NULL, warnings = FALSE)
    ), error = function(e)e
  )
  if(inherits(z, "error")){
    ticker <- gsub("\\.L", "", ticker)
    z <- tryCatch(
      suppressWarnings(
        getSymbols(ticker, env = NULL, warnings = FALSE)
      ), error = function(e)e)
  }
  if(inherits(z, "error") || nrow(z) == 0) return(empty_tibble())
  
  z <- z %>% 
    as_tibble() %>% 
    rownames_to_column(var = "Date") %>%
    mutate(Date = ymd(Date)) %>% 
    set_names(
      c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    ) %>% 
    mutate(
      Ticker = ticker,
      LogReturn = log_returns(Adjusted)
    ) %>% 
    filter(complete.cases(.))
  if(nrow(z) == 0) return(empty_tibble())
  z
}

#' Read stock prices from Yahoo Finance and update Azure SQL.
#' 
#' @inherit update_ftse_components
#' @inherit scrape_stock_price
#' 
#' @import DBI
#' 
#' @export
update_stock_price <- function(con, ticker, wait = 0.1){
  prices <- scrape_stock_price(ticker, wait = wait)
  ticker <- prices$Ticker[1]
  qry <- sprintf("DELETE FROM prices WHERE Ticker = '%s'", ticker)
  dbExecute(con, qry)
  z <- dbWriteTable(con, "prices", prices, append = TRUE)
  z
}

#' Read stock prices from Azure SQL.
#' 
#' @inherit update_ftse_components
#' @inherit scrape_stock_price
#' 
#' @export
get_stock_price <- function(con, ticker){
  cols <- "Date, Volume, Adjusted, Ticker, LogReturn"
  qry <- sprintf("SELECT %s FROM prices WHERE Ticker = '%s'", cols, ticker)
  rs <- dbSendQuery(con, qry)
  z <- dbFetch(rs)
  dbClearResult(rs)
  z
}

