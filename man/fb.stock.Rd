\name{fb.stock}
\alias{fb.stock}
\docType{data}
\title{
Facebook, Inc. Monthly Stock Prices Data Frame
}
\description{
Data frame containing data from stock prices in a monthly interval.
}
\usage{data(fb.stock)}
\format{
  A data frame with 46 observations on the following 7 variables.
  \describe{
    \item{\code{date}}{the date}
    \item{\code{open}}{the open price}
    \item{\code{high}}{the highest price}
    \item{\code{low}}{the lowest price}
    \item{\code{close}}{the close price}
    \item{\code{volume}}{the average volume of transactions}
    \item{\code{adj.close}}{the adjusted close price for dividends and splits}
  }
}
\details{
It contains stock prices data from may 18, 2012 to feb 1, 2016 of Facebook, Inc. (FB) traded on the NASDAQ. All prices are in USD.
}
\source{
\url{http://finance.yahoo.com/q/hp?s=FB+Historical+Prices}
}
\examples{
data(fb.stock)
}
\keyword{datasets}
