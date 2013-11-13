\name{markwear.acclist}
\alias{markwear.acclist}
\alias{markwear.accfile}

\title{
Classify wear and non-wear time
}
\description{
This functions identifies sequences of zeroes of a given length to classify wear and non-wear time in accelerometer data files.
}
\usage{
markwear.acclist(object, value, nz = 0, keep.error = FALSE)
markwear.accfile(object, value, nz = 0, keep.error = FALSE)
}

\arguments{
  \item{object}{
an object of class \code{acclist} or \code{accfile}.
}
  \item{value}{
the length of the time window (in minutes) which contains zero values.
}
  \item{nz}{
the length of the time window (in minutes) of non-zero value sequences allowed between every two sequences of zero values.
}

  \item{keep.error}{
logical flag. If \code{FALSE} (default) data errors as identified by \code{\link{errorCts}} will be replaced by NAs.
}

}
\details{
Currently, this function is implemented for class \code{gt1m} only. The accelerometer epoch is assumed to be expressed in seconds. Therefore \code{value} is automatically rescaled to \code{value * 60/object$info$epoch}.
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

data(gt1m_sample)

# 20-minute threshold
wear <- markwear.accfile(gt1m_sample, value = 20)
\dontrun{
> table(wear)
 wear
 Non-wear     Wear 
    27572    17338 
}

# lower threshold
wear <- markwear.accfile(gt1m_sample, value = 5)
\dontrun{
> table(wear)
 wear
 Non-wear     Wear 
    30188    14722 
}

# allow for some non-zero values within a 20-minute window
wear <- markwear.accfile(gt1m_sample, value = 20, nz = 2)
\dontrun{
> table(wear)
 wear
 Non-wear     Wear 
   28198    16712 
}

}

\keyword{wear time}
\keyword{ Actigraph GT1M }
