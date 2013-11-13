\name{handleError}
\alias{handleError}

\title{
Handling Errors
}
\description{
This function handles errors
}
\usage{
handleError(x, err, code = "all", replace = NULL, na = TRUE, keep.error = FALSE)
}

\arguments{
  \item{x}{
a vector of counts or steps.
}
  \item{err}{
a vector of error codes of the same length as \code{x}.
}
  \item{code}{
if \code{code = "all"} (default), any code different from zero will be flagged.
}
  \item{replace}{
value with which flagged errors are to be replaced. This argument is mandatory if \code{na = FALSE}.
}
  \item{na}{
if \code{TRUE} (default) flagged errors are replaced with NA's.
}
  \item{keep.error}{
if \code{TRUE} the function returns \code{x}. Otherwise, it will replace flagged errors as specified.
}
}

\value{
the vector \code{x} if \code{keep.error = TRUE} or the modified vector \code{x} if \code{keep.error = FALSE}.
}
\references{
Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}

\keyword{ accelerometer }
