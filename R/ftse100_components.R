#' Get list of FTSE 100 index components from wikipedia.
#' 
#' Scrapes "https://en.wikipedia.org/wiki/FTSE_100_Index" for list of index components
#' 
#' @return tibble
#' @export
#' 
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom dplyr mutate rename
#' @importFrom magrittr extract2
#' @importFrom tibble as_tibble
scrape_ftse100_components <- function(){
  Ticker <- NULL
  `FTSE Sector[14]` <- NULL
  ftse_page <- "https://en.wikipedia.org/wiki/FTSE_100_Index"
  ftse_page %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="constituents"]') %>% 
    html_table() %>% 
    extract2(1) %>% 
    mutate(
      Ticker = gsub("\\..*$", "", Ticker),
      Ticker = paste0(Ticker, ".L")
    ) %>% 
    rename(
      `FTSE Sector` = `FTSE Sector[14]`
    ) %>% 
    as_tibble()
}

#' Scrape FTSE component and wirte to database.
#' 
#' @param con database connection
#' @param tickers Stock tickers
#' 
#' @importFrom DBI dbWriteTable dbReadTable
update_ftse_components <- function(con, tickers){
  ftse <- scrape_ftse100_components()
  dbWriteTable(con, "ftse", ftse, overwrite = TRUE)
}

#' Read FTSE 100 components and tickers into data frame.
#' 
#' @inheritParams update_ftse_components
#' 
#' @return tibble
#' @export
get_ftse100_components <- function(con){
  dbReadTable(con, "ftse") %>% as_tibble()
}
