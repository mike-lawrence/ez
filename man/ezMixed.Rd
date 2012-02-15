\name{ezMixed}
\Rdversion{1.1}
\alias{ezMixed}
\title{
Provides assessment of fixed effects in an mixed effects modelling context.
}
\description{
This function provides assessment of fixed effects and their interactions via generalized mixed effects modelling, or generalized additive mixed effects modelling for effects involving numeric predictors to account for potentially non-linear effects of such predictors. See "Details" below for implementation notes.
}
\usage{
ezMixed(
	data
	, dv
	, random
	, fixed
	, add_quantile_as_fixed = FALSE
	, do_gam_for_numeric_fixed = TRUE
	, covariates = NULL
	, do_gam_for_numeric_covariates = TRUE
	, family = gaussian
	, gam_smooth = 'te'
	, gam_bs = 'ts'
	, gam_max_k_per_dim = Inf
	, alarm = TRUE
	, term_labels = NULL
	, highest = 0
	, return_models = TRUE
	, correction = AIC
	, progress_dir = NULL
	, resume = FALSE
	, parallelism = 'none'
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
Computation is achieved via \code{\link[lme4]{lmer}}, or \code{\link[gam]{mgcv}} when the effect under evaluation includes a numeric predictor. Assessment of each effect of interest necessitates building two models: (1) a "unrestricted" model that contains the effect of interest plus any lower order effects and (2) a "restricted" model that contains only the lower order effects (thus "restricting" the effect of interest to zero). These are then compared by means of a likelihood ratio, which needs to be corrected to account for the additional complexity of the unrestricted model relative to the restricted model. The default applied correction is Akaike's Information Criterion (AIC), which in the context of mixed effects models has been demonstrated to be asymptotically equivalent to cross-validation, a gold-standard technique for ensuring that model comparisons optimize prediction of new data.

The complexity-corrected likelihood ratio returned by \code{ezMixed} is represented on the log-base-2 scale, which has the following convenient properties:
\itemize{
\item (1) Resulting values can be discussed as representing "bits of evidence" for or against the evaluated effect.
\item (2) The bits scale permits easy representation of both very large and very small likelihood ratios.
\item (3) The bits scale represents equivalent evidence between the restricted and unrestricted models by a value of 0.
\item (4) The bits scale represents ratios favoring the restricted model symmetrically to those favoring the unrestricted model. That is, say one effect obtains a likelihood ratio of 8, and another effect obtains a likelihood ratio of 0.125; both ratios indicate the same degree of imbalance of evidence (8:1 and 1:8) and on the bits scale they faithfully represent this symmetry as values 3 and -3, respectively.
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
