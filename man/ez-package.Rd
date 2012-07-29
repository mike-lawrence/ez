\name{ez-package}
\alias{ez}
\docType{package}
\title{Easy analysis and visualization of factorial experiments}
\description{
This package facilitates easy analysis of factorial experiments, including purely within-Ss designs (a.k.a. \dQuote{repeated measures}), purely between-Ss designs, and mixed within-and-between-Ss designs. The functions in this package aim to provide simple, intuitive and consistent specification of data analysis and visualization. Visualization functions also include design visualization for pre-analysis data auditing, and correlation matrix visualization. Finally, this package includes functions for non-parametric analysis, including permutation tests and bootstrap resampling. The bootstrap function obtains predictions either by cell means or by more advanced/powerful mixed effects models, yielding predictions and confidence intervals that may be easily visualized at any level of the experiment's design.
}
\details{
\tabular{ll}{
Package: \tab ez\cr
Type: \tab Package\cr
Version: \tab 3.0-0\cr
Date: \tab 2011-02-05\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
This package contains several useful functions:
\itemize{
\item{\code{\link{ezANOVA}}}{Provides simple interface to ANOVA, including assumption checks.}
\item{\code{\link{ezBoot}}}{Computes bootstrap resampled cell means or lmer predictions}
\item{\code{\link{ezBootPlot}}}{When supplied the results from a call to \code{\link{ezBoot}}, plots predictions with bootstrapped confidence intervals.}
\item{\code{\link{ezCor}}}{Function to plot a correlation matrix with scatterplots, linear fits, and univariate density plots}
\item{\code{\link{ezDesign}}}{Function to plot a visual representation of the balance of data given a specified experimental design. Useful for diagnosing missing data issues.}
\item{\code{\link{ezMixed}}}{Provides assessment of fixed effects in a mixed effects modelling context.}
\item{\code{\link{ezPerm}}}{Provides simple interface to the Permutation test.}
\item{\code{\link{ezPlot}}}{Uses the \code{ggplot2} graphing package to generate plots for any given user-requested effect, by default producing error bars that facilitate visual post-hoc multiple comparisons.}
\item{\code{\link{ezPrecis}}}{Provides a summary of a given data frame.}
\item{\code{\link{ezPredict}}}{Computes predicted values from the fixed effects of a mixed effects model.}
\item{\code{\link{ezResample}}}{Resamples data, useful when bootstrapping.}
\item{\code{\link{ezStats}}}{Provides between-Ss descriptive statistics for any given user-requested effect.}
\item{\code{\link{progress_time}}}{A progress bar that estimates time remaining.}
\item{\code{\link{progress_timeCI}}}{A progress bar that estimates time remaining, including a bootstrapped confidence interval (useful when operations vary in duration).}
This package also contains two data sets:
\item{\code{\link{ANT}}}{Simulated data from the Attention Network Test}
\item{\code{\link{ANT2}}}{Messy version of the ANT data set}
}
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}
To report bugs or request features, please visit: \url{https://github.com/mike-lawrence/ez/issues}
To keep up to date on developments related to this package, join the discussion group at: \url{http://groups.google.com/group/ez4r}
}
\seealso{
\code{\link{ANT}}, \code{\link{ANT2}}, \code{\link{ezANOVA}}, \code{\link{ezBoot}}, \code{\link{ezBootPlot}}, \code{\link{ezCor}}, \code{\link{ezDesign}}, \code{\link{ezMixed}}, \code{link{ezMixedRel}}, \code{\link{ezPerm}}, \code{\link{ezPlot}}, \code{\link{ezPrecis}}, \code{\link{ezPredict}}, \code{\link{ezResample}}, \code{\link{ezStats}}, \code{\link{progress_time}}, \code{\link{progress_timeCI}}
}
\keyword{ package }
