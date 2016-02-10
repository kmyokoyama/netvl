\name{netvl.to.ts}
\alias{netvl.to.ts}

\title{
Transform column from a data set into a "ts" (Time-Series) object.
}
\description{
Take a data frame and returns a "ts" object (Time-Series) from one of its columns (variables).
As Financial Press Releases are released quaterly, the time-series is defined by quarter.
}
\usage{
netvl.to.ts(data, col.name, interval = "quarter")
}

\arguments{
  \item{data}{data frame containing a date variable (in format 4-digit year, month, day separated by -) and the variable to be transformed into a time-serie.}
  \item{col.name}{the name of the column to be transformed into a time-serie.}
  \item{interval}{the interval between observed data. It allows "quarter", "year", "month" and "day" options.}
}
\author{
Kazuki Yokoyama
}
\seealso{
\code{\link{ts}}
}
\examples{
library(netvl)
data(fb)
fb.maus <- netvl.to.ts(fb, "maus")
fb.revenue <- netvl.to.ts(fb, "revenue")
}

\keyword{ts}
