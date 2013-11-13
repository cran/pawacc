\name{plot.gt1m}
\alias{plot.gt1m}


\title{
Plot accelerometer file
}

\description{
This function plots data from an Actigraph GT1M accelerometer file.
}

\usage{
\method{plot}{gt1m}(x, y = NULL, xlab, ylab, main,
keep.error = TRUE, which = "counts", select = 1,...)
}

\arguments{
  \item{x}{
an object of class \code{gt1m}. It can be either \code{accfile} or \code{acclist}.
}
  \item{y}{
ignored.
}
  \item{xlab}{
x-axis label. If missing, 'Timestamp'.
}
  \item{ylab}{
y-axis label. If missing, 'counts' or 'steps'.
}
  \item{main}{
main title. If missing, 'fileid' from \code{x$info}.
}
  \item{keep.error}{
logical flag. If \code{FALSE} (default) data errors as identified by \code{\link{errorCts}} will be replaced by NAs.
}
  \item{which}{
either 'counts' or 'steps'.
}
  \item{select}{
numeric. If \code{class(x)} is \code{acclist}, this argument specifies the corresponding position of the accelerometer file in the \code{list} (first file by default).
}
  \item{...}{
Arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).
}


}


\references{
Actigraph (Pensacola, Florida).

Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}

\seealso{
\code{\link{plot}}, \code{\link{par}}, \code{\link{gt1m.accfile}}, \code{\link{gt1m.acclist}}
}

\examples{
data(gt1m_sample)

plot(gt1m_sample, which = "counts")
plot(gt1m_sample, which = "steps")

}

\keyword{ accelerometer }
\keyword{ Actigraph GT1M }
\keyword{ plot }