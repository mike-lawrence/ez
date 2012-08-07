\name{ezMixed}
\alias{ezMixed}
\title{Compute evidence for fixed effects in an mixed effects modelling context}
\description{
This function provides assessment of fixed effects and their interactions via generalized mixed effects modelling, or generalized additive mixed effects modelling for effects involving numeric predictors to account for potentially non-linear effects of such predictors. See Details section below for implementation notes.
}
\usage{
ezMixed(
    data
    , dv
    , family = gaussian
    , random
    , fixed
    , covariates = NULL
    , add_q = FALSE
    , fix_gam = TRUE
    , cov_gam = TRUE
    , gam_smooth = c('s','te')
    , gam_bs = 'ts'
    , gam_k = Inf
	, use_bam = FALSE
    , alarm = FALSE
    , term_labels = NULL
    , highest = Inf
    , return_models = TRUE
    , correction = AIC
    , progress_dir = NULL
    , resume = FALSE
    , parallelism = 'none'
    , gam_args = NULL
    , mer_args = NULL
)
}
\arguments{
\item{data}{
Data frame containing the data to be analyzed.
}
\item{dv}{
.() object specifying the column in \code{data} that contains the dependent variable. Values in this column must be numeric.
}
\item{family}{
Family to use to represent error. 
}
\item{random}{
.() object specifying one or more columns in \code{data} that contain random effects.
}
\item{fixed}{
.() object specifying one or more columns in \code{data} that contain fixed effects.
}
\item{covariates}{
.() object specifying one or more columns in \code{data} that contain variables to be used as fixed effect covariates.
}
\item{add_q}{
Logical. If TRUE, quantile values of each observation will be computed for each effect and interaction and these quantile values will be added as a fixed effect predictor. This permits investigating the effect of the fixed effect predictors specified in \code{fixed} on the shape of the distribution of residuals. Of course, this only really makes sense when there IS a distribution of residuals (i.e. not binomial data).
}
\item{fix_gam}{
Logical. If TRUE (default), generalized additive modelling is used to evaluate the possibly-non-linear effects of numeric fixed effect predictors.
}
\item{cov_gam}{
Logical. If TRUE (default), generalized additive modelling is used to represent the possibly-non-linear effects of numeric covariates.
}
\item{gam_smooth}{
Vector of one or more elements that are character strings specifying the name of the smoother to use when using gam. If a list of two elements, the first element will be used when evaluating effects and interactions that include a single numeric predictor, while the second element will be used when evaluating effects and interactions that involve multiple numeric predictors.
}
\item{gam_bs}{
Character specifying the name of the smooth term to use when using gam. 
}
\item{gam_k}{
Numeric value specifying the maximum value for \code{k} to supply to calls to gam. Higher values yield longer computation times but may better capture non-linear phenomena. If set to \code{Inf} (default), \code{ezMixed} will automatically use the maximum possible value for k given the number of unique combinations of values in the numeric predictors being evaluated. If a finite positive value is supplied, k will be set to that value or less (if the supplied k exceeds the maximum possible k for a given effect).
}
\item{use_bam}{
Logical. If TRUE, \code{\link[mgcv]{bam}} is used rather than \code{\link[mgcv]{gam}}.
}
\item{alarm}{
Logical. If TRUE, call the \code{\link[utils]{alarm}} function when \code{\link{ezMixed}} completes.
}
\item{term_labels}{
Vector of one or more elements that are character strings specifying effects to explore (useful when you want only a subset of all possible effects and interactions between the predictors supplied to the \code{fixed} argument).
}
\item{highest}{
Integer specifying the highest order interaction between the fixed effects to test. The default value, \code{Inf}, will test to the highest possible order.
}
\item{return_models}{
Logical. If TRUE, the returned list object will also include each lmer model (can become memory intensive for complex models and/or large data sets).
}
\item{correction}{
Name of a correction for complexity to apply (ex. AIC, BIC, etc) to each model's likelihood before computing likelihood ratios.
}
\item{progress_dir}{
Character string specifying name of a folder to be created to store results as they are computed (to save RAM).
}
\item{resume}{
Logical. If TRUE and a value is passed to the \code{progress_dir} argument, the progress directory will be searched for already completed effects and resume from these. Useful if a run was interrupted.
}
\item{parallelism}{
Character string specifying whether and how to compute models in parallel. If \dQuote{none}, no parallelism will be employed. If \dQuote{pair}, the restricted and unrestricted models for each effect will be computed in parallel (therefore using only 2 cores). If \dQuote{full}, then effects themselves will be computed in parallel (using all available cores). Parallelism assumes that a parallel backend has been specified (as in \code{library(doMC);options(cores=4);registerDoMC()}) and is likely only to work when running R from a unix terminal.
}
\item{gam_args}{
Single character string representing arguments to be passed to calls to \code{\link[mgcv]{gam}}.
}
\item{mer_args}{
Single character string representing arguments to be passed to calls to \code{\link[lme4]{lmer}} (or \code{\link[lme4]{glmer}} if the value to the \code{family} argument is not \code{gaussian}).
}
}
\details{
Computation is achieved via \code{\link[lme4]{lmer}}, or \code{\link[mgcv]{gam}} when the effect under evaluation includes a numeric predictor. Assessment of each effect of interest necessitates building two models: (1) a \dQuote{unrestricted} model that contains the effect of interest plus any lower order effects and (2) a \dQuote{restricted} model that contains only the lower order effects (thus \dQuote{restricting} the effect of interest to zero). These are then compared by means of a likelihood ratio, which needs to be corrected to account for the additional complexity of the unrestricted model relative to the restricted model. The default applied correction is Akaike's Information Criterion (AIC), which in the context of mixed effects models has been demonstrated to be asymptotically equivalent to cross-validation, a gold-standard technique for ensuring that model comparisons optimize prediction of new data.

The complexity-corrected likelihood ratio returned by \code{ezMixed} is represented on the log-base-2 scale, which has the following convenient properties:
\itemize{
\item (1) Resulting values can be discussed as representing \dQuote{bits of evidence} for or against the evaluated effect.
\item (2) The bits scale permits easy representation of both very large and very small likelihood ratios.
\item (3) The bits scale represents equivalent evidence between the restricted and unrestricted models by a value of 0.
\item (4) The bits scale represents ratios favoring the restricted model symmetrically to those favoring the unrestricted model. That is, say one effect obtains a likelihood ratio of 8, and another effect obtains a likelihood ratio of 0.125; both ratios indicate the same degree of imbalance of evidence (8:1 and 1:8) and on the bits scale they faithfully represent this symmetry as values 3 and -3, respectively.
}
}
\value{
A list with the following elements:
\item{summary}{A data frame summarizing the results, including whether warnings or errors occurred during the assessment of each effect and the bits of evidence associated with each.}
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
\references{
\itemize{
\item Glover S, Dixon P. (2004) Likelihood ratios: a simple and flexible statistic for empirical psychologists. Psychonomic Bulletin and Review, 11 (5), 791-806.
\item Fang, Y. (2011). Asymptotic equivalence between cross-validations and Akaike information criteria in mixed-effects models. Journal of the American Statistical Association, 9, 15-21.
}
}
\seealso{
\code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link[mgcv]{gam}}, \code{\link{ezMixedProgress}}, \code{\link{ezPredict}}, \code{\link{ezPlot2}}
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
