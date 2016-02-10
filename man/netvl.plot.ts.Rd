\name{netvl.plot.ts}
\alias{netvl.plot.ts}

\title{
Plotting Time-Series objects
}
\description{
Plotting method for data frames with date and another variable of interest.
}
\usage{
netvl.plot.ts(data, ts.type = NULL, ylab = NULL, title = NULL)
}

\arguments{
  \item{data}{data frame with date formatted variable and another variable of interest or a time-series 'ts' object.}
  \item{ts.type}{variable of interest. It can be "maus", "revenue" or "costs". If 'data' is a 'ts' object, then ts.type must be NULL.}
  \item{ylab}{Vertical axis (y) label.}
  \item{title}{Main title label.}
}
\author{Kazuki Yokoyama}
\seealso{
\code{\link{ggplot} which is used to actually plot.}
}
\examples{
data(fb)
netvl.plot.ts(fb, "maus")
netvl.plot.ts(fb, "revenue")
netvl.plot.ts(fb, "costs")
maus <- netvl.to.ts(fb, "maus")
netvl.plot.ts(maus, ylab = "Million of users", title = "Monthly Active Users")
}

\keyword{ts}
