\name{markbouts.acclist}
\alias{markbouts}
\alias{markbouts.accfile}
\alias{markbouts.acclist}

\title{
Classify accumulation of physical activity in bouts
}
\description{
This function identifies bouts of physical activity using user-defined breakpoints for accelerometer counts.
}
\usage{
markbouts(object, value, which = "counts", bts = c(0, 10, 20, Inf), rescale.epoch = 60,
collapse.by = "\%Y-\%m-\%d", value.labels = NULL, bouts.labels = NULL,
extreme = NULL, keep.error = FALSE)
\method{markbouts}{acclist}(object, value, which = "counts", bts = c(0, 10, 20, Inf), rescale.epoch = 60,
collapse.by = "\%Y-\%m-\%d", value.labels = NULL, bouts.labels = NULL,
extreme = NULL, keep.error = FALSE)
\method{markbouts}{accfile}(object, value, which = "counts", bts = c(0, 10, 20, Inf), rescale.epoch = 60,
collapse.by = "\%Y-\%m-\%d", value.labels = NULL, bouts.labels = NULL,
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
  \item{bts}{
vector of breaks to define bouts duration (in minutes).
}
  \item{rescale.epoch}{
epoch expressed in the same unit as acceleromenter's epoch to determine multiplier to rescale \code{value} (default is 60). See details.
}
  \item{collapse.by}{
dataset aggregation level. See argument \code{format} from \code{\link{strptime}} for options and details below.
}
  \item{value.labels}{
labels for physical activity modes.
}
  \item{bouts.labels}{
labels for bouts duration catergories (\code{NULL} is recommended).
}
  \item{extreme}{
%%   if \code{value} includes a threshold for defining the category of extreme values, this argument identifies such category among the physical activity modes defined by \code{value}. See details.
}
  \item{keep.error}{
logical flag. If \code{FALSE} (default) data errors as identified by \code{\link{errorChk}} will be replaced by NAs.
}
}
\details{
Breakpoints are specified as counts per seconds using \code{value}. If the epoch used for \code{value} is different from the accelerometer's epoch, a rescaling is applied. E.g., if epoch is 15 seconds and breakpoints are expressed as counts per 60 seconds, \code{value} is divided by 60/15 = 4. There can be \emph{n} physical activity modes at maximum, where \emph{n} is the length of \code{value}. 

The argument \code{extreme} is \code{NULL} by default. Use either `last' to select the last category or the category number 1 to \emph{n}.
}
\value{
The function \code{markbouts.accfile} returns duration, frequency and mean duration of bouts by bout category, physical activity mode and \code{T} levels of \code{collapse.by} (e.g., day) in array of dimension \code{c(length(bts) - 1,length(value) + 1,3,T)}. \code{markbouts.acclist} is applied to objects of class \code{acclist}, in which case a list of arrays of the same length as the number of accelerometer files in \code{object} is returned.
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
