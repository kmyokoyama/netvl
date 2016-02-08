\name{netvl.plot.ts}
\alias{netvl.plot.ts}

\title{
Plotting Time-Series objects
}
\description{
Plotting method for data frames with date and another variable of interest.
}
\usage{
netvl.plot.ts(data, ts.type)
}

\arguments{
  \item{data}{data frame with date formatted variable and another variable of interest.}
  \item{ts.type}{variable of interest. It can be "maus", "revenue" or "costs".}
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
}

\keyword{ts}
