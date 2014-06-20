\name{errorChk}
\alias{errorChk}


\title{
Perform error checking
}
\description{
These functions look for errors in the data. A code is returned.
}
\usage{
errorChk(x, fault = 32767)
}

\arguments{
  \item{x}{
vector of accelerometer data.
}
  \item{fault}{
numerical value that indicates voltage signal saturation (temporarily used for both accelerometer counts and steps).
  }
}
\details{
Error coded are as follow: 0, no error; 1, all values are 5-digit values or all one value;  2, negative values; 3, \code{NA}s.
}

\value{
a vector of the same length as \code{x}.
}

\references{
Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}



\seealso{
\code{\link{gt1mAccFile}}, \code{\link{gt3xAccFile}}
}

\keyword{ error }
\keyword{ Actigraph GT1M }
\keyword{ Actigraph GT3X }
