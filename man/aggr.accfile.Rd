\name{agg.accfile}
\alias{agg.accfile}

\title{
Aggregate Accelerometry Data
}
\description{
This function aggregates count values 
}

\usage{
agg.accfile(object, by, x = NULL, y = NULL,
	keep.error = FALSE)
}

\arguments{
  \item{object}{
an object of class \code{accfile}.
  }
  \item{by}{
epoch by which count and steps are aggregated. Note: it cannot be less that the accelerometer epoch (\code{object$info$epoch
}).
}
  \item{x}{
optional argument. If \code{NULL}, this is set to counts (\code{object$counts}).
}
  \item{y}{
optional argument. If \code{NULL}, this is set to counts (\code{object$steps}).
}
  \item{keep.error}{
logical flag. Should errors be omitted?
}

}


\value{
\item{counts }{aggregated counts}
\item{steps }{aggregated steps}
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
agg.accfile(gt1m_sample, by = 30)

# aggregate by 5-minute epochs
agg.accfile(gt1m_sample, by = 300, keep.error = FALSE)

}

\keyword{ collapse }
