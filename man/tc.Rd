\name{tc}
\alias{tc}
\docType{data}
\title{
Tencent Data Frame
}
\description{
Data frame containing data of Date, Monthly Active Users (MAUs) and Revenue of Tencent.
}
\usage{data(tc)}
\format{
  A data frame with 46 observations on the following 3 variables.
  \describe{
    \item{\code{date}}{the date}
    \item{\code{maus}}{monthly active users average for the quarter.}
    \item{\code{revenue}}{GAAP revenue for the quarter}
  }
}
\details{
It contains data from the second quarter 2004 (2004'Q2) to the third quarter 2015 (2015'Q3) obtained from the financial releases reported to investors.
The Date (date) variable is formatted as "\%Y-\%m-\%d".
Monthly Active Users (maus) variable is in million of users.
Revenue (revenue) is in million USD.
}
\source{
\url{http://www.tencent.com/en-us/ir/news/2015.shtml}
}
\examples{
data(tc)
}
\keyword{datasets}
