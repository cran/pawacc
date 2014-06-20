\name{plot.gt1m}
\alias{plot.gt3x}
\alias{plot.gt1m}


\title{
Plot accelerometer file
}

\description{
Thes functions plot data from Actigraph GT1M and GT3X accelerometer files.
}

\usage{
\method{plot}{gt1m}(x, y = NULL, xlab, ylab, main,
keep.error = TRUE, which = "counts", select = 1,...)
\method{plot}{gt3x}(x, y = NULL, xlab, ylab, main,
keep.error = TRUE, which = "x", select = 1,...)
}

\arguments{
  \item{x}{
an object of class \code{gt1m}. It can be either \code{accfile} or \code{acclist}.
}
  \item{y}{
ignored.
}
  \item{xlab}{
x-axis label (optional).
}
  \item{ylab}{
y-axis label (optional).
}
  \item{main}{
main title (optional).
}
  \item{keep.error}{
logical flag. If \code{FALSE} (default) data errors as identified by \code{\link{errorChk}} will be replaced by NAs.
}
  \item{which}{
either 'counts' or 'steps' for \code{gt1m} files or one of \code{c('x','y','z','steps')} for \code{gt3x} files.
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
\code{\link{plot}}, \code{\link{par}}, \code{\link{gt1mAccFile}}, \code{\link{gt1mAccDir}}
}

\examples{
data(gt1m_sample)
data(gt3x_sample)


plot(gt1m_sample, which = "counts")
plot(gt1m_sample, which = "steps")
plot(gt3x_sample, which = "x")
plot(gt3x_sample, which = "steps")


}

\keyword{ accelerometer }
\keyword{ Actigraph GT1M }
\keyword{ plot }