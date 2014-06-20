\name{gt1mAccDir}
\alias{gt1mAccDir}

\title{
Read list of accelerometer files
}
\description{
This function reads a list of Actigraph GT1M accelerometer files.
}
\usage{
gt1mAccDir(accFileList, save, compress = "gzip",
	compression_level = 6)
}

\arguments{
  \item{accFileList}{
an object of type \code{list}.
 }
  \item{save}{
either logical or character. See \code{\link{readAccDir}} for details.
}
  \item{compress}{
logical or character string specifying whether saving to a named file is to use compression if \code{save = TRUE}. See argument \code{compress} in \code{\link{save}}.
}
  \item{compression_level}{
integer: the level of compression to be used. See argument \code{compression_level} in \code{\link{save}}.
}
}

\value{
An object of class \code{acclist}.
}

\references{
Actigraph (Pensacola, Florida).

Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}

}

\author{
Marco Geraci
}


\seealso{
\code{\link{readAccDir}}
}

\keyword{ accelerometer }
\keyword{ Actigraph GT1M }
