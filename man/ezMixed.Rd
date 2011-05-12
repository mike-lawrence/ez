\name{ezMixed}
\Rdversion{1.1}
\alias{ezMixed}
\title{
Provides assessment of fixed effects in an mixed effects modelling context.
}
\description{
This function provide assessment of fixed effects and their interactions via mixed effects modelling, including automated assessment of non-linearity via polynomials. Models are built using \code{\link[lme4]{lmer}} and permit specification of multiple random effects.
}
\usage{
ezMixed(
	data
	, dv
	, random
	, fixed
	, fixed_poly = NULL
	, fixed_poly_max = NULL
	, family = gaussian
	, alarm = TRUE
	, results_as_progress = FALSE
	, highest = 0
	, return_models = FALSE
	, highest_first = TRUE
)
}
\arguments{
  \item{data}{
Data frame containing the data to be analyzed.
}
  \item{dv}{
.() object specifying the column in \code{data} that contains the dependent variable. Values in this column must be numeric.
}
  \item{random}{
.() object specifying one or more columns in \code{data} that contain random effects.
}
  \item{fixed}{
.() object specifying one or more columns in \code{data} that contain fixed effects.
}
  \item{fixed_poly}{
Optional .() object specifying one or more columns in \code{data} that are already specified in \code{fixed} and contain fixed effects to be fit with polynomials.
}
  \item{fixed_poly_max}{
Optional numeric vector with the same length as \code{fixed_poly} specifying the maximum polynomial degree for each corresponding variable supplied to \code{fixed_poly}. Otherwise, the default maximum degree is 5.
}
  \item{family}{
 a description of the error distribution and link function to be used in the model. This can be a character string naming a family function, a family function or the result of a call to a family function. See \code{\link[stats]{family}}.
}
  \item{alarm}{
Logical. If TRUE (default), call the \code{\link[utils]{alarm}} function when \code{\link{ezMixed}} completes.
}
  \item{results_as_progress}{
Logical. Default is FALSE, but if TRUE, results are printed as they are computed (useful for very long computations).
}
  \item{highest}{
Integer specifying the highest order interaction between the fixed effects to test. The default value, 0, will test to the highest possible order.
}
  \item{return_models}{
Logical. If TRUE, the returned list object will also include each lmer model (can become memory intensive for complex models and/or large data sets).
}
  \item{highest_first}{
Logical. Default is TRUE, which computes the models for the highest order interaction first.  
}
}
\details{
Computation is achived via \code{\link[lme4]{lmer}}. Assessment of each effect of interest necessitates building two models: (1) a "unrestricted" model that contains the effect of interest plus any lower order effects and (2) a "restricted" model that contains only the lower order effects (thus "restricting" the effect of interest to zero). These are then compared by means of a likelihood ratio, which needs to be corrected to account for the additional complexity of the unrestricted model relative to the restricted model. This correction can be achieved by either the Akaike Information Criterion (AIC) or the Bayesian Information Criterion (BIC), two equally well-defined but differently motivated approaches to accounting for model complexity. Generally, the BIC imposes a stronger penalty for complexity, yielding corrected likelihood ratios that are more likely to favor the restricted model. Users are encouraged to refer to Kuha (2004, listed in references below) for discussion of the different motivations of AIC and BIC to help decide which suits their application best.

Both complexity-corrected variants of the likelihood ratio returned by \code{ezMixed} are transformed to the log-base-10 scale (also known as the Bel scale), which has the following convenient properties:
\itemize{
\item (1) The Bel scale permits easy representation of both very large and very small likelihood ratios.
\item (2) The Bel scale represents equivalent evidence between the restricted and unrestricted models by a value of 0.
\item (3) The Bel scale represents ratios favoring the restricted model symmetrically to those favoring the unrestricted model. That is, say one effect obtains a likelihood ratio of 10, and another effect obtains a likelihood ratio of .1; both ratios indicate the same degree of imbalance of evidence (10:1 and 1:10) and on the Bel scale they faithfully represent this symmetry as values 1 and -1, respectively.
\item (4) While any logarithmich transform achieves the above benefits, the Bel scale is particularly useful because integer differences on the Bel scale represent changes by an order of magnitude on the raw likelihood ratio scale (eg. 1 Bel corresponds to a likelihood ratio of 1, 2 Bels corresponds to a likelihood ratio of 100, etc.).
}
}
\value{
A list with the following elements:
\item{summary}{A data frame summarizing the results, including whether warnings or errors occurred during the assessment of each effect, raw natural-log likelihood of the unrestricted and restricted models (RLnLu and RLnLr, respectively), degrees of freedom of the unrestricted and restricted models (DFu and DFr, respectively), and log-base-10 likelihood ratios corrected via AIC and BIC (L10LRa and L10LRb, respectively)}
\item{formulae}{A list of lists, each named for an effect and containing two elements named "unrestricted" and "restricted", which in turn contain the right-hand-side formulae used to fit the unrestricted and restricted models, respectively.}
\item{errors}{A list similar to \code{formulae}, but instead storing errors encountered in fitting each model.}
\item{warnings}{A list similar to \code{formulae}, but instead storing warnings encountered in fitting each model.}
\item{models}{(If requested by setting \code{return_models=TRUE}) A list similar to \code{formulae} but instead storing each fitted model. }
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}
To report bugs or request features, please visit: \url{https://github.com/mike-lawrence/ez/issues}
To keep up to date on developments related to this package, join the discussion group at: \url{http://groups.google.com/group/ez4r}
}
\references{
\itemize{
\item Kuha, J. (2004). AIC and BIC : Comparisons of Assumptions and Performance. Sociological Methods & Research, 33, p188.
}
}
\seealso{
    \code{\link[lme4]{lmer}}, \code{\link{ANT}}, \code{\link{ANT2}}, \code{\link{ezANOVA}}, \code{\link{ezBoot}}, \code{\link{ezBootPlot}}, \code{\link{ezCor}}, \code{\link{ezDesign}}, \code{\link{ezMixed}}, \code{link{ezMixedRel}}, \code{\link{ezPerm}}, \code{\link{ezPlot}}, \code{\link{ezPrecis}}, \code{\link{ezPredict}}, \code{\link{ezResample}}, \code{\link{ezStats}}, \code{\link{progress_time}}, \code{\link{progress_timeCI}}
}
\examples{
#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)
ezPrecis(ANT)

#Run ezMixed on the accurate RT data
rt = ezMixed(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , random = .(subnum)
    , fixed = .(cue,flank,group)
)
print(rt$summary)

\dontrun{
#Run ezMixed on the error rate data
er = ezMixed(
    data = ANT
    , dv = .(error)
    , random = .(subnum)
    , fixed = .(cue,flank,group)
    , family = 'binomial'
)
print(er$summary)
}
}
