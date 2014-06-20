\name{markwear.acclist}
\alias{markwear}
\alias{markwear.accfile}
\alias{markwear.acclist}

\title{
Classify wear and non-wear time
}
\description{
This functions identifies sequences of zeroes of a given length to classify wear and non-wear time in accelerometer data files.
}
\usage{
markwear(object, value, which = "counts", rescale.epoch = 60,
	nz = 0, keep.error = FALSE)
\method{markwear}{acclist}(object, value, which = "counts", rescale.epoch = 60,
	nz = 0, keep.error = FALSE)
\method{markwear}{accfile}(object, value, which = "counts", rescale.epoch = 60,
	nz = 0, keep.error = FALSE)
}

\arguments{
  \item{object}{
an object of class \code{acclist} or \code{accfile}.
}
  \item{value}{
the length of the time window (in minutes) which contains zero values.
}
  \item{which}{
either 'counts' or 'steps' for \code{gt1m} files or one of \code{c('x','y','z','steps')} for \code{gt3x} files.
}
  \item{rescale.epoch}{
epoch expressed in the same unit as acceleromenter epoch to determine multiplier to rescale \code{value} (default is 60). See details.
}
  \item{nz}{
the length of the time window (in minutes) of non-zero value sequences allowed between every two sequences of zero values.
}

  \item{keep.error}{
logical flag. If \code{FALSE} (default) data errors as identified by \code{\link{errorChk}} will be replaced by NAs.
}

}
\details{
The accelerometer epoch is assumed to be expressed in seconds. Therefore \code{value} is automatically rescaled to \code{value * 60/object$info$epoch}.
}

\value{
If \code{object} is of class \code{acclist}, a list of factors with two levels of the same length as the number of accelerometers files in \code{object}. If \code{object} is of class \code{accfile}, a single factor will be given. Levels are 'non-wear' and 'wear'.
}
\references{
Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}


\seealso{
\code{\link{readAccDir}}
}

\examples{

\dontrun{

data(gt1m_sample)

# 20-minute threshold
wear <- markwear(gt1m_sample, value = 20)
> table(wear)
 wear
 Non-wear     Wear 
    27572    17338 
}

\dontrun{

# lower threshold
wear <- markwear(gt1m_sample, value = 5)
> table(wear)
 wear
 Non-wear     Wear 
    30188    14722 
}

\dontrun{

# allow for some non-zero values within a 20-minute window
wear <- markwear.accfile(gt1m_sample, value = 20, nz = 2)
> table(wear)
 wear
 Non-wear     Wear 
   28198    16712 
}

}

\keyword{wear time}
\keyword{ Actigraph GT1M }
\keyword{ Actigraph GT3X }
