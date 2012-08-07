\name{ANT}
\alias{ANT}
\docType{data}
\title{ANT data}
\description{
\emph{Simulated} data from then Attention Network Test (see reference below), consisting of 2 within-Ss variables (\dQuote{cue} and \dQuote{flank}), 1 between-Ss variable (\dQuote{group}) and 2 dependent variables (response time, \dQuote{rt}, and whether an error was made, \dQuote{error})
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
Michael A. Lawrence \email{mike.lwrnc@gmail.com}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
}
\examples{
data(ANT)
head(ANT)
ezPrecis(ANT)
}
\keyword{datasets}
