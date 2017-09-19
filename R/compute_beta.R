#' Compute the beta between a stock and the market.
#' 
#' @param x Stock data. This must be a data frame or tibble with columns for `Date` and `LogReturn`
#' @param m Market data. This must be a data frame or tibble with columns for `Date` and `LogReturn`
#' @param days Number of days of history to consider
#' @param mean_reversion A value between 0 and 1, indicating how much mean reversion to take into account.  The beta will be adjusted using the formula `1 * mean_reversion + beta * (1 - mean_reversion)`
#' @return Numeric value
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select left_join starts_with
compute_beta <- function(x, m, days = 365, mean_reversion = 0){
  Date <- NULL
  z <- x %>% left_join(m, by = "Date") %>% 
    select(Date, starts_with("LogReturn")) %>% 
    tail(days)
  x <- z$LogReturn.x
  m <- z$LogReturn.y
  beta <- cov(x, m, use = "pairwise.complete.obs") / var(m)
  1 * mean_reversion + beta * (1 - mean_reversion)
}
