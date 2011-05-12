\name{ezANOVA}
\Rdversion{1.1}
\alias{ezANOVA}
\title{
Function to perform a factorial ANOVA
}
\description{
This function provides easy analysis of data from factorial experiments, including purely within-Ss designs (a.k.a. "repeated measures"), purely between-Ss designs, and mixed within-and-between-Ss designs, yielding ANOVA results and assumption checks.
}
\usage{
ezANOVA(
	data
	, dv
	, wid
	, within = NULL
	, between = NULL
	, observed = NULL
	, diff = NULL
	, reverse_diff = FALSE
	, type = 2
	, white.adjust = FALSE
	, detailed = FALSE
	, return_aov = FALSE
)
}
\arguments{
  \item{data}{
Data frame containing the data to be analyzed.
}
  \item{dv}{
.() object specifying the column in \code{data} that contains the dependent variable. Values in this column must be numeric.
}
  \item{wid}{
.() object specifying the column in \code{data} that contains the variable specifying the case/Ss identifier.
}
  \item{within}{
Optional .() object specifying one or more columns in \code{data} that contain predictor variables that are manipulated (or observed) within-Ss.
}
  \item{between}{
Optional .() object specifying one or more columns in \code{data} that contain predictor variables that are manipulated (or observed) between-Ss.
}
  \item{observed}{
Optional .() object specifying one or more columns in \code{data} that are already specified in either \code{within} or \code{between} that contain predictor variables that are observed variables (i.e. not manipulated). The presence of observed variables affects the computation of the generalized eta-squared measure of effect size reported by \code{\link{ezANOVA}}.
}
  \item{diff}{
Optional .() object specifying a 2-level within-Ss varbiable to collapse to a difference score.
}
  \item{reverse_diff}{
Logical. If TRUE, triggers reversal of the difference collapse requested by \code{diff}.
}
  \item{type}{
Numeric value (either \code{1}, \code{2} or \code{3}) specifying the Sums of Squares "type" to employ when data are unbalanced (eg. when group sizes differ). \code{type = 2} is the default because this will yield identical ANOVA results as \code{type = 1} when data are balanced but \code{type = 2} will additionally yield various assumption tests where appropriate. When data are unbalanced, users are warned that they should give special consideration to the value of \code{type}. \code{type=3} will emulate the approach taken by popular commercial statistics packages like SAS and SPSS, but users are warned that this approach is not without criticism.
}
  \item{white.adjust}{
Only affects behaviour if the design contains only between-Ss predictor variables. If not FALSE, the value is passed as the white.adjust argument to \code{\link[car]{Anova}}, which provides heteroscedasticity correction. See \code{\link[car]{Anova}} for details on possible values.
}
  \item{detailed}{
Logical. If TRUE, returns extra information (sums of squares, raw likelihood ratios, AIC & BIC values used in computing corrected likelihood ratios, etc.).
}
  \item{return_aov}{
Logical. If TRUE, computes and returns an aov object corresponding to the requested ANOVA (useful for computing post-hoc contrasts).
}
}
\details{
While \code{within} and \code{between} are both optional, at least one column of \code{data} must be provided to either \code{within} or \code{between}. Prior to running, \code{dv} is collapsed to a mean for each cell defined by the combination of \code{wid} and any variables supplied to \code{within} and/or \code{between} and/or \code{diff}. 
}
\value{
A list containing one or more of the following components:
\item{ANOVA}{A data frame containing the ANOVA results.}
\item{Mauchly's Test for Sphericity}{If any within-Ss variables with >2 levels are present, a data frame containing the results of Mauchly's test for Sphericity. Only reported for effects >2 levels because sphericity necessarily holds for effects with only 2 levels.}
\item{Sphericity Corrections}{If any within-Ss variables are present, a data frame containing the Greenhouse-Geisser & Huynh-Feldt epsilon values, and corresponding corrected p-values.}
\item{Levene's Test for Homgeneity}{If the design is purely between-Ss, a data frame containing the results of Levene's test for Homgeneity of variance. Note that Huynh-Feldt corrected p-values where the Huynh-Feldt epsilon >1 will use 1 as the correction epsilon.}
\item{aov}{An aov object corresponding to the requested ANOVA.}
}
\note{
Some column names in the output data frames are abbreviated to conserve space:
\tabular{ll}{
DFn \tab Degrees of Freedom in the numerator (a.k.a. DFeffect).\cr
DFd \tab Degrees of Freedom in the denominator (a.k.a. DFerror).\cr
SSn \tab Sum of Squares in the numerator (a.k.a. SSeffect).\cr
SSd \tab Sum of Squares in the denominator (a.k.a. SSerror).\cr
F \tab F-value.\cr
p \tab p-value (probability of the data given the null hypothesis).\cr
p<.05 \tab Highlights p-values less than the traditional alpha level of .05.\cr
ges \tab Generalized Eta-Squared measure of effect size (see in references below: Bakeman, 2005).\cr
GGe \tab Greenhouse-Geisser epsilon.\cr
p[GGe] \tab p-value after correction using Greenhouse-Geisser epsilon.\cr
p[GGe]<.05 \tab Highlights p-values (after correction using Greenhouse-Geisser epsilon) less than the traditional alpha level of .05.\cr
HFe \tab Huynh-Feldt epsilon.\cr
p[HFe] \tab p-value after correction using Huynh-Feldt epsilon.\cr
p[HFe]<.05 \tab Highlights p-values (after correction using Huynh-Feldt epsilon) less than the traditional alpha level of .05.\cr
W \tab Mauchly's W statistic
}
}
\references{
\itemize{
\item Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. Behavior Research Methods, 37 (3), 379-384.
\item Glover S, Dixon P. (2004) Likelihood ratios: a simple and flexible statistic for empirical psychologists. Psychonomic bulletin & review, 11 (5), 791-806.
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
\examples{
#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)
ezPrecis(ANT)


#Run an ANOVA on the mean correct RT data.
rt_anova = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , wid = .(subnum)
    , within = .(cue,flank)
    , between = .(group)
)

#Show the ANOVA & assumption tests.
print(rt_anova)

#Run an ANOVA on the mean_rt data, ignoring group.
rt_anova2 = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , wid = .(subnum)
    , within = .(cue,flank)
)

#Show the ANOVA & assumption tests.
print(rt_anova2)

#Run a purely between-Ss ANOVA on the mean_rt data.
rt_anova3 = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , wid = .(subnum)
    , between = .(group)
)

#Show the ANOVA & assumption tests.
print(rt_anova3)
}
