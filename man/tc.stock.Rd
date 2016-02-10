\name{tc.stock}
\alias{tc.stock}
\docType{data}
\title{
Tencent Holdings Limited Monthly Stock Prices Data Frame
}
\description{
Data frame containing data from stock prices in an monthly interval.
}
\usage{data(tc.stock)}
\format{
  A data frame with 141 observations on the following 7 variables.
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
It contains stock prices data from aug 31, 2010 to feb 1, 2016 of Tencent Holdings Limited (0700.HK) traded on the HDKSE. All prices are in USD.
}
\source{
\url{https://finance.yahoo.com/q/hp?s=0700.HK+Historical+Prices}
}
\examples{
data(tc.stock)
}
\keyword{datasets}
