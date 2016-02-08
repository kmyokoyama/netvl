\name{netvl.read.csv}
\alias{netvl.read.csv}

\title{
Read CSV files containing data.
}
\description{
Read CSV files containing formatted data. It must contain header and at least the variable "date"
in the format "%Y-%m-%d" (ie, 4-digit year, month and day, separated by "-").
}
\usage{
netvl.read.csv(file.name)
}
\arguments{
  \item{file.name}{The name of the file with the data. Likely it will be a .csv file.}
}
\author{
Kazuki Yokoyama
}
\seealso{\code{\link{read.csv}}}
\keyword{read.csv}
