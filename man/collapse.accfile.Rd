\name{collapse.accfile}
\alias{collapse}
\alias{collapse.accfile}

\title{
Collapse raw accelerometer files into a summary dataset
}
\description{
This function collapses accelerometer data into a dataframe with summary statistics.
}
\usage{
collapse(...)
\method{collapse}{accfile}(object, which = "counts", palist = list(value = c(0, 100, 1000, 5000, 13000),
rescale.epoch = 60, labels = NULL, extreme = NULL), mwlist = list(value = 20,
nz = 0, rescale.epoch = 60), collapse.by = "\%Y-\%m-\%d", collapse.epoch = 60, aggregate.by =
NULL, FUN.list = list(mean = function(x) mean(x, na.rm = TRUE)),
keep.extreme = FALSE, keep.error = FALSE, ...)
}

\arguments{
  \item{object}{
an object of class \code{gt1m}.
}
  \item{which}{
either 'counts' or 'steps' for \code{gt1m} files or one of \code{c('x','y','z','steps')} for \code{gt3x} files.
}
  \item{palist}{
list of arguments for \code{\link{markpa.accfile}}.
}
  \item{mwlist}{
list of arguments for \code{\link{markwear.accfile}}.
}
  \item{collapse.by}{
dataset aggregation level. See argument \code{format} from \code{\link{strptime}} for options and details below.
}
  \item{collapse.epoch}{
epoch by which time spent in different physical activity modes is summarized. See details.
}
  \item{aggregate.by}{
pre-collapsing aggregation level for accelerometer values. See argument \code{format} from \code{\link{strptime}} for options.
}

  \item{FUN.list}{
a named list of functions. See \code{\link{fun.collapse}}.
}
  \item{keep.extreme}{
logical flag. If \code{FALSE} (default) extreme values will be replaced by NAs. See \code{\link{markpa.accfile}}.
}
  \item{keep.error}{
logical flag. If \code{FALSE} (default) data errors as identified by \code{\link{errorChk}} will be replaced by NAs.
}
  \item{\dots}{
arguments for \code{\link{dateSummary}}.
}
}



\value{
A data frame containing the following variables
\item{collapse.by}{aggregation factor}
\item{fileid}{file identifier}
\item{...}{named columns according to arguments \code{FUN} and \code{labels} of \code{palist}}
}

\references{
Geraci M, Rich C, Sera F, Cortina-Borja M, Griffiths LJ, and Dezateux C (2012). Technical report on accelerometry data processing in the Millennium Cohort Study. London, UK: University College London. Available at \url{http://discovery.ucl.ac.uk/1361699}
}

\author{
Marco Geraci
}

\seealso{
\code{\link{markpa.accfile}}, \code{\link{markwear.accfile}}, \code{\link{fun.collapse}}
}

\examples{
\dontrun{

data(gt1m_sample)

collapse(gt1m_sample, palist = list(value = c(0, 100, 2000, 4000, 11000),
rescale.epoch = 60, labels = c("sedentary","light","moderate","vigorous","extreme_values"),
extreme = "last"), mwlist = list(value = 20,
nz = 0), collapse.by = "\%Y-\%m-\%d", collapse.epoch = 60,
FUN.list = list(mean = function(x) round(mean(x, na.rm = TRUE),2)),
keep.extreme = FALSE, keep.error = FALSE)
$outcome
  collapse.by fileid outcome.mean sedentary  light moderate vigorous
1  2011-12-08   test       117.63    293.75 243.75    29.75     4.00
2  2011-12-09   test       157.83    349.75 143.25    33.50    24.50
3  2011-12-10   test        79.75    468.25 177.25    24.25     6.25
4  2011-12-11   test        57.96    355.50 126.00     8.50     3.25
5  2011-12-12   test        70.05    455.50 157.25    19.50     6.00
6  2011-12-13   test        72.99    475.50 181.50    15.25     8.00
7  2011-12-14   test        79.94    476.25 210.50    20.25     8.50
8  2011-12-15   test       232.50      0.00   0.50     0.00     0.00
9  2011-12-16   test          NaN      0.00   0.00     0.00     0.00
  extreme_values non-wear
1           0.00   388.75
2           6.00   883.00
3           0.50   763.50
4           0.25   946.50
5           0.00   801.75
6           0.25   759.50
7           1.50   723.00
8           0.00  1439.50
9           0.00   187.50

$call
collapse.accfile(object = gt1m_sample, palist = list(value = c(0, 
    100, 2000, 4000, 11000), rescale.epoch = 60, labels = c("sedentary", 
    "light", "moderate", "vigorous", "extreme_values"), extreme = "last"), 
    mwlist = list(value = 20, nz = 0), collapse.by = "%Y-%m-%d", 
    collapse.epoch = 60, FUN.list = list(mean = function(x) round(mean(x, 
        na.rm = TRUE), 2)), keep.extreme = FALSE, keep.error = FALSE)

attr(,"class")
[1] "accfile.collapse"
Warning message:
In collapse.accfile(gt1m_sample, palist = list(value = c(0, 100,  :
  NAs imputed where extreme counts found
}

\dontrun{

collapse(gt1m_sample, palist = list(value = c(0, 100, 2000, 4000, 11000),
rescale.epoch = 60, labels = c("sedentary","light","moderate","vigorous","extreme_values"),
extreme = "last"), mwlist = list(value = 20,
nz = 0), collapse.by = "\%Y-\%m-\%d", collapse.epoch = 60,
FUN.list = list(mean = function(x) round(mean(x, na.rm = TRUE),2),
sd = function(x) round(sd(x, na.rm = TRUE),2),
"95th" = function(x) round(quantile(x, probs = .95, na.rm = TRUE),2)),
keep.extreme = TRUE, keep.error = FALSE)
$outcome
  collapse.by fileid outcome.mean outcome.sd outcome.95th sedentary  light
1  2011-12-08   test       117.63     216.12       529.40    293.75 243.75
2  2011-12-09   test       201.10     567.65      1085.60    349.75 143.25
3  2011-12-10   test        81.97     221.33       465.50    468.25 177.25
4  2011-12-11   test        59.80     172.08       320.35    355.50 126.00
5  2011-12-12   test        70.05     188.49       401.00    455.50 157.25
6  2011-12-13   test        74.08     207.55       386.85    475.50 181.50
7  2011-12-14   test        87.42     275.27       415.30    476.25 210.50
8  2011-12-15   test       232.50     222.74       374.25      0.00   0.50
9  2011-12-16   test          NaN         NA           NA      0.00   0.00
  moderate vigorous extreme_values non-wear
1    29.75     4.00           0.00   388.75
2    33.50    24.50           6.00   883.00
3    24.25     6.25           0.50   763.50
4     8.50     3.25           0.25   946.50
5    19.50     6.00           0.00   801.75
6    15.25     8.00           0.25   759.50
7    20.25     8.50           1.50   723.00
8     0.00     0.00           0.00  1439.50
9     0.00     0.00           0.00   187.50

$call
collapse.accfile(object = gt1m_sample, palist = list(value = c(0, 
    100, 2000, 4000, 11000), rescale.epoch = 60, labels = c("sedentary", 
    "light", "moderate", "vigorous", "extreme_values"), extreme = "last"), 
    mwlist = list(value = 20, nz = 0), collapse.by = "%Y-%m-%d", 
    collapse.epoch = 60, FUN.list = list(mean = function(x) round(mean(x, 
        na.rm = TRUE), 2), sd = function(x) round(sd(x, na.rm = TRUE), 
        2), `95th` = function(x) round(quantile(x, probs = 0.95, 
        na.rm = TRUE), 2)), keep.extreme = TRUE, keep.error = FALSE)

attr(,"class")
[1] "accfile.collapse"
}


}

\keyword{ collapse }
\keyword{ Actigraph GT1M }
\keyword{ Actigraph GT3X }
