\name{markpa.acclist}
\alias{markpa}
\alias{markpa.accfile}
\alias{markpa.acclist}

\title{
Classify mode of physical activity
}
\description{
This function identifies modes of physical activity using user-defined breakpoints for accelerometer counts.
}
\usage{
markpa(object, value, which = "counts", rescale.epoch = 60, labels = NULL,
extreme = NULL, keep.error = FALSE)
\method{markpa}{acclist}(object, value, which = "counts", rescale.epoch = 60, labels = NULL,
extreme = NULL, keep.error = FALSE)
\method{markpa}{accfile}(object, value, which = "counts", rescale.epoch = 60, labels = NULL,
extreme = NULL, keep.error = FALSE)
}

\arguments{
  \item{object}{
an object of class \code{acclist} or \code{accfile}.
}
  \item{value}{
vector of breaks to define physical activity modes. The lowest breakpoint must be 0.
}
  \item{which}{
either 'counts' or 'steps' for \code{gt1m} files or one of \code{c('x','y','z','steps')} for \code{gt3x} files.
}
  \item{rescale.epoch}{
epoch expressed in the same unit as acceleromenter epoch to determine multiplier to rescale \code{value} (default is 60). See details.
}
  \item{labels}{
labels for physical activity modes.
}
  \item{extreme}{
if \code{value} includes a threshold for defining the category of extreme values, this argument identifies such category among the physical activity modes defined by \code{value}. See details.
}

  \item{keep.error}{
logical flag. If \code{FALSE} (default) data errors as identified by \code{\link{errorChk}} will be replaced by NAs.
}

}

\details{
Breakpoints are specified as counts per seconds using \code{value}. If the epoch used for \code{value} is different from the accelerometer epoch, a rescaling is applied. E.g., if epoch is 15 seconds and breakpoints are expressed as counts per 60 seconds, \code{value} is divided by 60/15 = 4. There can be \emph{n} physical activity modes at maximum, where \emph{n} is the length of \code{value}. 

The argument \code{extreme} is \code{NULL} by default. Use either `last' to select the last category or the category number 1 to \emph{n}.
}

\value{
If \code{object} is of class \code{acclist}, a list of factors of the same length as the number of accelerometer files in \code{object}. If \code{object} is of class \code{accfile}, a single factor will be given. The number of levels is equal to \code{length(value) + 1}.
}

\author{
Marco Geraci
}

\seealso{
\code{\link{readAccDir}}
}

\keyword{physical activity mode}
\keyword{ Actigraph GT1M }
\keyword{ Actigraph GT3X }
