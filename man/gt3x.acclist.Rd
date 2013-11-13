\name{gt3x.acclist}
\alias{gt3x.acclist}

\title{
Read list of accelerometer files
}
\description{
This function reads a list of Actigraph GT3X accelerometer files.
}
\usage{
gt3x.acclist(accFileList, save, compress = "gzip",
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
}

\author{
Marco Geraci
}


\seealso{
\code{\link{readAccDir}}
}

\keyword{ accelerometer }
\keyword{ Actigraph GT3X }
