\name{tsFromEpoch}
\alias{tsFromEpoch}
\alias{epochFromTS}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate timestamp from epoch number or epoch number from timestamp
}
\description{
Utility functions.
}
\usage{
tsFromEpoch(object, x)
epochFromTS(object, x)
}

\arguments{
  \item{object}{
an object of class \code{accfile}.
  }

  \item{x}{
either an integer giving the epoch number or the timestamp in a \code{POSIX} format (e.g., '\%Y-\%m-\%d \%H:\%M:\%S').
}
}


\value{
either a timestamp corresponding to an epoch number or the epoch number corresponding to a timestamp.
}

\author{
Marco Geraci
}


\seealso{
\code{\link{gt1mAccFile}}
}

\examples{
data(gt1m_sample)

tsFromEpoch(gt1m_sample, 10000)
# [1] "2011-12-10 01:39:45 GMT"

epochFromTS(gt1m_sample, as.POSIXlt(strptime('2011-12-10 01:39:45', '\%Y-\%m-\%d \%H:\%M:\%S')))
# [1] 10000
}

\keyword{ timestamp }
\keyword{ Actigraph GT1M }
\keyword{ Actigraph GT3X }
