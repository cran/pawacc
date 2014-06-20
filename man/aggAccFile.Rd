\name{aggAccFile}
\alias{aggAccFile}

\title{
Aggregate Accelerometry Data
}
\description{
This function aggregates count values 
}

\usage{
aggAccFile(object, by, which = "counts",
	x = NULL, keep.error = FALSE)
}

\arguments{
  \item{object}{
an object of class \code{accfile}.
  }
  \item{by}{
epoch by which count and steps are aggregated. Note: it cannot be less that the accelerometer epoch (\code{object$info$epoch}).
}
  \item{which}{
either 'counts' or 'steps' for \code{gt1m} files or one of \code{c('x','y','z','steps')} for \code{gt3x} files.
}
  \item{x}{
optional argument. If \code{NULL}, this is set to \code{counts}.
}
  \item{keep.error}{
logical flag. Should errors be omitted?
}

}


\value{
\item{outcome}{aggregated values}
\item{ts_agg }{time stamping}
}

\references{
Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}

\seealso{
\code{\link{collapse.accfile}}
}

\examples{

data(gt1m_sample)

# aggregate by 30-second epochs
aggAccFile(gt1m_sample, by = 30)

# aggregate by 5-minute epochs
aggAccFile(gt1m_sample, by = 300, keep.error = FALSE)

}

\keyword{ collapse }
