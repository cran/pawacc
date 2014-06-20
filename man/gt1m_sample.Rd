\name{gt1m_sample}
\alias{gt1m_sample}
\title{GT1M accelerometer file}
\description{
  This is the output of \code{\link{gt1mAccFile}}.
}

\format{
 See \code{value} in \code{\link{gt1mAccFile}}. The 'raw' file 'gt1m_sample.dat' can be found in the directory '\\inst\\extdata' of this package.
}

\source{
Actigraph (Pensacola, Florida).
}

\examples{
\dontrun{
gt1m_sample <- gt1mAccFile("gt1m_sample.dat", path = "", fileid = "test")
gt1m_sample
  fileid serial  nobs epoch mode            ts_start  tz voltage               ts_dl
1   test  xxxxx 44910    15    1 2011-12-08 08:00:00 GMT    3.76 2011-12-24 11:20:36
}
}

\keyword{datasets}
\keyword{ Actigraph GT1M }
