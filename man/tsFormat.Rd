\name{tsFormat}
\alias{tsFormat}

\title{
Time Stamping
}

\description{
Time stamping.
}
\usage{
tsFormat(object)
}

\arguments{
  \item{object}{
an object of class \code{accfile}.
}
}

\value{
a vector of timestamps.
}

\author{
Marco Geraci
}

\seealso{
\code{\link{tsFromEpoch}}
}

\examples{

data(gt1m_sample)
data(gt3x_sample)

tsFormat(gt1m_sample)
tsFormat(gt3x_sample)

}

\keyword{ timestamp }
\keyword{ Actigraph GT1M }
\keyword{ Actigraph GT3X }
