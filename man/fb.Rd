\name{fb}
\alias{fb}
\docType{data}
\title{
Facebook Data Frame
}
\description{
Data frame containing data of Date, Monthly Active Users (MAUs), Revenue and Total Costs of Facebook Co.
}
\usage{data(fb)}
\format{
  A data frame with 15 observations on the following 4 variables.
  \describe{
    \item{\code{date}}{the date}
    \item{\code{maus}}{monthly active users average for the quarter.}
    \item{\code{revenue}}{GAAP revenue for the quarter}
    \item{\code{costs}}{GAAP total costs for the quarter}
  }
}
\details{
It contains data from the second quarter 2012 (2012'Q2) to the fourth quarter 2015 (2015'Q4) obtained from the financial releases reported to investors.
The Date (date) variable is formatted as "\%Y-\%m-\%d".
Monthly Active Users (maus) variable is in million of users.
Revenue (revenue) is in million USD.
Total costs (costs) is in million USD.
}
\source{
\url{http://investor.fb.com/releases.cfm}
}
\examples{
data(fb)
}
\keyword{datasets}
