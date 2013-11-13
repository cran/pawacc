\name{gt3x_sample}
\alias{gt3x_sample}
\title{GT3X accelerometer file}
\description{
  This is the output of \code{\link{gt3x.accfile}}.
}

\format{
 See \code{value} in \code{\link{gt3x.accfile}}. The 'raw' file 'gt3x_sample.dat' can be found in the directory '\\inst\\extdata' of this package.
}

\source{
Actigraph (Pensacola, Florida).
}

\examples{
\dontrun{
gt3x_sample <- gt3x.accfile("gt3x_sample.dat", path = "", fileid = "test")
gt3x_sample
  fileid serial nobs epoch mode            ts_start  tz voltage               ts_dl
1   test  xxxxx 2676     1   13 2009-03-03 10:40:00 GMT    4.09 2009-03-03 11:24:49
}
}

\keyword{datasets}
