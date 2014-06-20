\name{gt3xAccFile}
\alias{gt3xAccFile}

\title{
Read a single accelerometer file
}

\description{
This function reads Actigraph GT3X and ActiSleep accelerometer files.
}

\usage{
gt3xAccFile(file, path, fileid, tz = "Europe/London",
	sparse = FALSE, fault = 32767)

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
  \item{fault}{
numerical value that indicates voltage signal saturation.
}

}

\details{
Raw accelerometer data are processed according to the device data format. See file 'gt3x_sample.dat' in directory '\\inst\\extdata' of this package.
}

\value{
These functions return an object of two \code{\link{class}}es: \code{accfile} and additional device-specific class (i.e., \code{gt3x}).

An object of class \code{accfile} is a list containing the following components:

\item{df}{A \code{\link{data.frame}} object with accelerometer values in columns \code{y}, \code{x}, \code{z}, and \code{steps} (if present), and coded error for each accelerometer data column. See \code{\link{errorChk}} for error codes. If \code{sparse = TRUE}, all variables of the data frame \code{df} are returned as vectors of a matrix in sparse format (see \code{\link{as.matrix.csr}} for details).}
\item{info}{A \code{\link{data.frame}} object with file identifier (\code{fileid}), device serial number (\code{serial}), number of recorded measurements (\code{nobs}), epoch (\code{epoch}), accelerometer mode (\code{mode}), start date and time (\code{ts_start}), time zone (\code{tz}), battery voltage (\code{voltage}), download date and time (\code{ts_dl}).}
\item{error_summary}{A \code{\link{list}} object with file identifier (\code{fileid}), summary tables of error codes for each accelerometer data column, error code for date (\code{date}), and logical flag for odd number of measurements (\code{odd_number}) (see details).}

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

\examples{

data(gt3x_sample)
class(gt3x_sample)

}

\keyword{ accelerometer }
\keyword{ Actigraph GT3X }
\keyword{ Actigraph ActiSleep }
\keyword{ read file }