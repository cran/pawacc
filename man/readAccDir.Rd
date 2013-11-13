\name{readAccDir}
\alias{readAccDir}

\title{
Read content of a directory with accelerometer files
}
\description{
This function reads raw files downloaded from accelerometers and stores them in one directory.
}
\usage{
readAccDir(path, model, ext = "dat", counts.pos = 1,
	tz = "Europe/London", sparse = FALSE,
	fault = 32767, save = TRUE, compress = "gzip",
	compression_level = 6,...)
}

\arguments{
  \item{path}{
path to accelerometer files.
 }
  \item{model}{
accelerometer model, either 'gt1m' or 'gt3x'.
}
  \item{ext}{
file extension without '.' (default is 'dat').
}
  \item{counts.pos}{
position of first measurement of accelerometer counts. See \code{\link{gt1m.accfile}} for details.
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
  \item{save}{
either logical or character. If logical, \code{save = TRUE} stores accelerometer files as \code{.Rdata} objects in a new directory ('accRfiles') in \code{path}. If the directory already exists, confirmation for overwriting will be prompted. \code{save = FALSE} stores accelerometer files in current R session. Alternatively, an existing folder can be specified.
}
  \item{compress}{
logical or character string specifying whether saving to a named file is to use compression if \code{save = TRUE}. See argument \code{compress} in \code{\link{save}}.
}
  \item{compression_level}{
integer: the level of compression to be used. See argument \code{compression_level} in \code{\link{save}}.
}
  \item{\dots}{
not used.
}
}
\details{
This is a wrapper function. It reads raw accelerometer files stored in one directory. The argument \code{ext} specifies the extension of the files to look for (e.g., 'dat' or 'txt'). Files with different extension and/or sub-directories are ignored. The data format must be consistent with the specification of \code{model}.
}
\value{
Either a set of \code{.Rdata} files or a \code{\link{list}} of objects of class \code{accfile}. In both cases, each accelerometer data file is stored as an object of type \code{list} and labelled using the data file name. See \code{\link{gt1m.accfile}} or \code{\link{gt3x.accfile}} for details.

}

\references{
Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}


\seealso{
\code{\link{gt1m.accfile}}, \code{\link{gt3x.accfile}}, \code{\link{gt1m.acclist}}, \code{\link{gt3x.acclist}}
}

\keyword{ accelerometer }
\keyword{ Actigraph GT1M }
\keyword{ Actigraph GT3X }
\keyword{ read file }
