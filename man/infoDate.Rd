\name{infoDate}
\alias{infoDate}

\title{
Perform date formatting and checking
}
\description{
This function returns accelerometer date in 'year-month-day' format and a code.

}
\usage{
infoDate(x, id)
}

\arguments{
  \item{x}{
character vector with day, month, and year returned by accelerometer.
}
  \item{id}{
file identifier.
}
}

\value{

\item{ date }{Date in 'y/m/d' format.}
\item{ date_code}{One of the following: 0, original date in 'y/m/d' format; 1, original date in 'y/d/m'; 2, date format not recognised.
}

}
\references{
Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}


\seealso{
\code{\link{gt1m.accfile}}
}

\keyword{ date }
\keyword{ accelerometer }

