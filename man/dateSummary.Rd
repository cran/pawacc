\name{dateSummary}
\alias{dateSummary}

\title{
Date summary for accelerometer files
}
\description{
This function provides a date summary for Actigraph GT1M accelerometer files.
}
\usage{
dateSummary(object, wear, timestamp, minval = 0,
rescale.epoch = 60, keep.error = FALSE)
}
\arguments{
  \item{object}{
an object of class \code{gt1m}.
}
  \item{wear}{
a vector that classifies wear and non-wear time. See \code{\link{markwear.accfile}}.
}
  \item{timestamp}{
a timestamp vector for accelerometer values that can be provided by \code{tsFormat}.
}
  \item{minval}{
threshold defining the minimum number of minutes to identify first and last days. See details.
}
  \item{rescale.epoch}{
epoch expressed in the same unit as acceleromenter's epoch to determine minutes of wear time (default is 60 and assumed to be in seconds).
 
}

  \item{keep.error}{
logical flag. If \code{FALSE} (default) data errors as identified by \code{\link{errorChk}} will be replaced by NAs.
}
}

\details{
Based on total wear time (in minutes) for each day, the theshold \code{minval} is applied to identify the first and last days. For example, if accelerometers are sent by post to collect survey data, the first and last days in which the accelerometer was worn might not be known. Days before the first and those after the last day (\code{truncated} days) are discarded by \code{\link{collapse.accfile}}.
}

\value{A data frame containing the following variables
\item{fileid}{file identifier}
\item{days}{dates by calendar day}
\item{freq}{frequency of accelerometer observations in each day}
\item{hour_day}{total hours of accelerometer observations in each day}
\item{start_day}{starting time of accelerometer observations}
\item{end}{end time of accelerometer observations}
\item{valid_mins}{wear time (minutes)}
\item{IsStartDate}{dummy variable to define starting date (1 = yes)}
\item{IsEndDate}{dummy variable to define end date (1 = yes)}
\item{IsTruncated}{dummy variable to define truncated date (1 = yes)}
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

mw <- markwear.accfile(gt1m_sample, 20)
tsf <- tsFormat(gt1m_sample)
dateSummary(gt1m_sample, mw, tsf)
\dontrun{
  fileid       days freq hour_day start_day end_day valid_mins IsStartDate
1   test 2011-12-08 3840   16.000        08      23     571.25           1
2   test 2011-12-09 5760   24.000        00      23     557.00           0
3   test 2011-12-10 5760   24.000        00      23     676.50           0
4   test 2011-12-11 5760   24.000        00      23     493.50           0
5   test 2011-12-12 5760   24.000        00      23     638.25           0
6   test 2011-12-13 5760   24.000        00      23     680.50           0
7   test 2011-12-14 5760   24.000        00      23     717.00           0
8   test 2011-12-15 5760   24.000        00      23       0.50           0
9   test 2011-12-16  750    3.125        00      03       0.00           0
  IsEndDate IsTruncated
1         0           0
2         0           0
3         0           0
4         0           0
5         0           0
6         0           0
7         0           0
8         0           0
9         1           0
}

# at least 600 minutes per day to determine first and last day
dateSummary(gt1m_sample, mw, tsf, minval = 600)
\dontrun{
  fileid       days freq hour_day start_day end_day valid_mins IsStartDate
1   test 2011-12-08 3840   16.000        08      23     571.25           0
2   test 2011-12-09 5760   24.000        00      23     557.00           0
3   test 2011-12-10 5760   24.000        00      23     676.50           1
4   test 2011-12-11 5760   24.000        00      23     493.50           0
5   test 2011-12-12 5760   24.000        00      23     638.25           0
6   test 2011-12-13 5760   24.000        00      23     680.50           0
7   test 2011-12-14 5760   24.000        00      23     717.00           0
8   test 2011-12-15 5760   24.000        00      23       0.50           0
9   test 2011-12-16  750    3.125        00      03       0.00           0
  IsEndDate IsTruncated
1         0           1
2         0           1
3         0           0
4         0           0
5         0           0
6         0           0
7         1           0
8         0           1
9         0           1
}

}

\keyword{ Actigraph GT1M }
\keyword{ summary }
