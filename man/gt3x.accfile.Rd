\name{gt3x.accfile}
\alias{gt3x.accfile}

\title{
Read a single accelerometer file
}

\description{
This function reads an Actigraph GT3X accelerometer file.
}

\usage{
gt3x.accfile(file, path, fileid, tz = "Europe/London",
	sparse = FALSE)

}

\arguments{
  \item{file}{
file name including file extension.
}
  \item{path}{
path to \code{file}.
}
  \item{fileid}{
label for file identifier.
}
  \item{tz}{
a character string specifying the timezone to be used for the conversion (see \code{\link{strptime}}).
}
  \item{sparse}{
logical flag: should data be stored in sparse format?
}

}

\details{
Raw accelerometer data are processed according to the device data format. See file 'gt3x_sample.dat' in directory '\\inst\\extdata' of this package.
}

\value{
These functions return an object of two \code{\link{class}}es: \code{accfile} and additional device-specific class (i.e., \code{gt3x}).

An object of class \code{accfile} is a list containing the following components:

\item{df}{A \code{\link{data.frame}} object with file identifier ('fileid'), accelerometer values in columns 'x', 'y', 'z', 'w'. If \code{sparse = TRUE}, a matrix named \code{Data} with accelerometer values is returned in sparse matrix format (see \code{\link{as.matrix.csr}} for details).}
\item{info}{A \code{\link{data.frame}} object with file identifier ('fileid'), number of recorded measurements ('nobs'), epoch ('epoch'), accelerometer mode ('mode'), start date and time ('TimeStamp_orig'), time zone ('TZ'), battery voltage ('voltage'), download date and time ('TimeStamp_dl').}
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

\examples{

data(gt3x_sample)
class(gt3x_sample)

}

\keyword{ accelerometer }
\keyword{ Actigraph GT3X }
\keyword{ read file }