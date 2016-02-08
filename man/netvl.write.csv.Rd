\name{netvl.write.csv}
\alias{netvl.write.csv}

\title{
Write data frame to CSV files.
}
\description{
Write R data frame to CSV file.
}
\usage{
netvl.write.csv(data, file.name)
}
\arguments{
  \item{data}{Data frame to be written.}
  \item{file.name}{The name of the file to be created. Likely it will be a .csv file.}
}
\author{
Kazuki Yokoyama
}
\seealso{\code{\link{write.table}}}
\examples{
library(netvl)
data(fb)
data(tc)
netvl.write.csv(fb, file.name = "facebook.csv")
netvl.write.csv(tc, file.name = "tencent.csv")
}
\keyword{write.table}
