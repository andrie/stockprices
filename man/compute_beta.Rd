% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_beta.R
\name{compute_beta}
\alias{compute_beta}
\title{Compute the beta between a stock and the market.}
\usage{
compute_beta(x, m, days = 365, mean_reversion = 0)
}
\arguments{
\item{x}{Stock data. This must be a data frame or tibble with columns for \code{Date} and \code{LogReturn}}

\item{m}{Market data. This must be a data frame or tibble with columns for \code{Date} and \code{LogReturn}}

\item{days}{Number of days of history to consider}

\item{mean_reversion}{A value between 0 and 1, indicating how much mean reversion to take into account.  The beta will be adjusted using the formula \code{1 * mean_reversion + beta * (1 - mean_reversion)}}
}
\value{
Numeric value
}
\description{
Compute the beta between a stock and the market.
}
