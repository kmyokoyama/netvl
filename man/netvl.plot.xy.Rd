\name{netvl.plot.xy}
\alias{netvl.plot.xy}

\title{
Plotting variables against Monthly Active Users.
}
\description{
Plotting method for data frames with maus and another variable of interest.
}
\usage{
netvl.plot.xy(data, y.type, regression = TRUE)
}

\arguments{
  \item{data}{data frame with Monthly Active Users (maus) variable and another variable of interest.}
  \item{y.type}{variable of interest. It can be "revenue" or "costs".}
  \item{regression}{TRUE if a regression line with CI should be drawn and FALSE otherwise.}
}
\author{Kazuki Yokoyama}
\seealso{
\code{\link{ggplot} which is used to actually plot.}
}
\examples{
library(netvl)
data(fb)
netvl.plot.xy(fb, "revenue")
netvl.plot.xy(fb, "costs")
}

\keyword{ts}
