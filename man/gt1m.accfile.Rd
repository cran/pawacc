\name{gt1m.accfile}
\alias{gt1m.accfile}


\title{
Read a single accelerometer file
}

\description{
This function reads an Actigraph GT1M accelerometer file.
}

\usage{
gt1m.accfile(file, path, fileid, counts.pos = 1,
	tz = "Europe/London", sparse = FALSE,
	fault = 32767)
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
  \item{counts.pos}{
when storage mode allows for accelerometer counts and steps to be recorded at the same time, this argument specifies the position of first measurement of accelerometer counts (default is \code{counts.pos} = 1).
}
  \item{tz}{
a character string specifying the timezone to be used for the conversion (see \code{\link{strptime}}).
}
  \item{sparse}{
logical flag: should data be stored in sparse format?
}
  \item{fault}{
numerical value that indicates voltage signal saturation (temporarily used for both accelerometer counts and steps).
}

}

\details{
Raw accelerometer data are processed according to the device data format. Several data checks are performed by \code{\link{errorCts}},  \code{\link{errorSteps}}, and \code{\link{infoDate}}. An additional check is performed on the lenght of the sequence of measurements when both accelerometer counts and steps are recorded. If the length is odd, a warning message is produced. See file 'gt1m_sample.dat' in directory '\\inst\\extdata' of this package.
}

\value{
These functions return an object of two \code{\link{class}}es: \code{accfile} and additional device-specific class (i.e., \code{gt1m}).

An object of class \code{accfile} is a list containing the following components:

\item{df}{A \code{\link{data.frame}} object with file identifier (\code{fileid}), accelerometer counts (\code{counts}), accelerometer steps (\code{steps}), coded error for counts (\code{error_c}) and steps (\code{error_s}). See \code{\link{errorCts}} for error codes. If \code{sparse = TRUE}, all variables except \code{fileid} of the data frame \code{df} are returned as elements of the main list with corresponding names but in sparse matrix format (see \code{\link{as.matrix.csr}} for details).}
\item{info}{A \code{\link{data.frame}} object with file identifier (\code{fileid}), number of recorded measurements (\code{nobs}), epoch (\code{epoch}), accelerometer mode (\code{mode}), start date and time (\code{ts_start}), time zone (\code{tz}), battery voltage (\code{voltage}), download date and time (\code{ts_dl}).}
\item{error_summary}{A \code{\link{list}} object with file identifier (\code{fileid}), summary tables of error codes for counts (\code{counts}) and steps (\code{steps}), error code for date (\code{date}), and logical flag for odd number of measurements (\code{odd_number}) (see details).}
}


\references{
Actigraph (Pensacola, Florida).

Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}

\seealso{
\code{\link{readAccDir}}, \code{\link{gt1m.acclist}}
}

\examples{

data(gt1m_sample)
class(gt1m_sample)

}


\keyword{ accelerometer }
\keyword{ Actigraph GT1M }
\keyword{ read file }