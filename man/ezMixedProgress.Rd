\name{ezMixedProgress}
\alias{ezMixedProgress}
\title{Retrieve information saved to file by a call to ezMixed}
\description{
When running \code{ezMixed} with a value supplied to the \code{progress_dir} argument, summary results are saved to file. \code{ezMixedProgress} retrieves those results, even from partial or discontinued runs.
}
\usage{
ezMixedProgress(
    progress_dir
    , return_models = TRUE
)
}
\arguments{
\item{progress_dir}{
Character string specifying the name of the progress directory. (Should match the string supplied as the value to the \code{progress_dir} argument in the original call to \code{ezMixed})
}
\item{return_models}{
Logical. If TRUE, the returned list object will also include each lmer model (can become memory intensive for complex models and/or large data sets).
}
}
\value{
A list with the following elements:
\item{summary}{A data frame summarizing the results, including whether warnings or errors occurred during the assessment of each effect, raw natural-log likelihood of the unrestricted and restricted models (RLnLu and RLnLr, respectively), degrees of freedom of the unrestricted and restricted models (DFu and DFr, respectively), and log-base-10 likelihood ratios corrected via AIC and BIC (L10LRa and L10LRb, respectively)}
\item{formulae}{A list of lists, each named for an effect and containing two elements named \dQuote{unrestricted} and \dQuote{restricted}, which in turn contain the right-hand-side formulae used to fit the unrestricted and restricted models, respectively.}
\item{errors}{A list similar to \code{formulae}, but instead storing errors encountered in fitting each model.}
\item{warnings}{A list similar to \code{formulae}, but instead storing warnings encountered in fitting each model.}
\item{models}{(If requested by setting \code{return_models=TRUE}) A list similar to \code{formulae} but instead storing each fitted model. }
}
\author{
Michael A. Lawrence \email{mike.lwrnc@gmail.com}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
}
\seealso{
\code{\link{ezMixed}}
}
\examples{
#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)
ezPrecis(ANT)

#Run ezMixed on the accurate RT data
rt_mix = ezMixed(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , random = .(subnum)
    , fixed = .(cue,flank,group)
    , progress_dir = 'rt_mix'
)

rt_mix = ezMixedProgress('rt_mix')
print(rt_mix$summary)

}
