\name{ANT}
\Rdversion{1.1}
\alias{ANT}
\docType{data}
\title{
ANT data
}
\description{
\emph{Simulated} data from then Attention Network Test (see reference below), consisting of 2 within-Ss variables ("cue" and "flank"), 1 between-Ss variable ("group") and 2 dependent variables (response time, "rt", and whether an error was made, "error")
}
\usage{data(ANT)}
\format{
  A data frame with 5760 observations on the following 10 variables.
  \describe{
    \item{\code{subnum}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17} \code{18} \code{19} \code{20}}
    \item{\code{group}}{a factor with levels \code{Control} \code{Treatment}}
    \item{\code{block}}{a numeric vector}
    \item{\code{trial}}{a numeric vector}
    \item{\code{cue}}{a factor with levels \code{None} \code{Center} \code{Double} \code{Spatial}}
    \item{\code{flank}}{a factor with levels \code{Neutral} \code{Congruent} \code{Incongruent}}
    \item{\code{location}}{a factor with levels \code{down} \code{up}}
    \item{\code{direction}}{a factor with levels \code{left} \code{right}}
    \item{\code{rt}}{a numeric vector}
    \item{\code{error}}{a numeric vector}
  }
}
\references{
J Fan, BD McCandliss, T Sommer, A Raz, MI Posner (2002). Testing the efficiency and independence of attentional networks. \emph{Journal of Cognitive Neuroscience}, \bold{14}, 340-347.
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}
To report bugs or request features, please visit: \url{https://github.com/mike-lawrence/ez/issues}
To keep up to date on developments related to this package, join the discussion group at: \url{http://groups.google.com/group/ez4r}
}
\seealso{
    \code{\link{ANT}}, \code{\link{ANT2}}, \code{\link{ezANOVA}}, \code{\link{ezBoot}}, \code{\link{ezBootPlot}}, \code{\link{ezCor}}, \code{\link{ezDesign}}, \code{\link{ezMixed}}, \code{link{ezMixedRel}}, \code{\link{ezPerm}}, \code{\link{ezPlot}}, \code{\link{ezPrecis}}, \code{\link{ezPredict}}, \code{\link{ezResample}}, \code{\link{ezStats}}, \code{\link{progress_time}}, \code{\link{progress_timeCI}}
}
\examples{
data(ANT)
head(ANT)
ezPrecis(ANT)
}
\keyword{datasets}
