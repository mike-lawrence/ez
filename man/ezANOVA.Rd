\name{ezANOVA}
\alias{ezANOVA}
\title{Compute ANOVA}
\description{
This function provides easy analysis of data from factorial experiments, including purely within-Ss designs (a.k.a. \dQuote{repeated measures}), purely between-Ss designs, and mixed within-and-between-Ss designs, yielding ANOVA results, generalized effect sizes and assumption checks.
}
\usage{
ezANOVA(
    data
    , dv
    , wid
    , within = NULL
    , within_full = NULL
    , within_covariates = NULL
    , between = NULL
    , between_covariates = NULL
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
Name of the column in \code{data} that contains the dependent variable. Values in this column must be numeric.
}
\item{wid}{
Name of the column in \code{data} that contains the variable specifying the case/Ss identifier. This should be a unique value per case/Ss.
}
\item{within}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) within-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{within_full}{
Same as within, but intended to specify the full within-Ss design in cases where the data have not already been collapsed to means per condition specified by \code{within} and when \code{within} only specifies a subset of the full design.
}
\item{within_covariates}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) within-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{between}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) between-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{between_covariates}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) between-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{observed}{
Names of columns in \code{data} that are already specified in either \code{within} or \code{between} that contain predictor variables that are observed variables (i.e. not manipulated). If a single value, may be specified by name alone; if multiple values, must be specified as a .() list. The presence of observed variables affects the computation of the generalized eta-squared measure of effect size reported by \code{\link{ezANOVA}}.
}
\item{diff}{
Names of any variables to collapse to a difference score. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list. All supplied variables must be factors, ideally with only two levels (especially if setting the \code{reverse_diff} argument to TRUE).
}
\item{reverse_diff}{
Logical. If TRUE, triggers reversal of the difference collapse requested by \code{diff}. Take care with variables with more than 2 levels.
}
\item{type}{
Numeric value (either \code{1}, \code{2} or \code{3}) specifying the Sums of Squares \dQuote{type} to employ when data are unbalanced (eg. when group sizes differ). \code{type = 2} is the default because this will yield identical ANOVA results as \code{type = 1} when data are balanced but \code{type = 2} will additionally yield various assumption tests where appropriate. When data are unbalanced, users are warned that they should give special consideration to the value of \code{type}. \code{type=3} will emulate the approach taken by popular commercial statistics packages like SAS and SPSS, but users are warned that this approach is not without criticism.
}
\item{white.adjust}{
Only affects behaviour if the design contains only between-Ss predictor variables. If not FALSE, the value is passed as the white.adjust argument to \code{\link[car]{Anova}}, which provides heteroscedasticity correction. See \code{\link[car]{Anova}} for details on possible values.
}
\item{detailed}{
Logical. If TRUE, returns extra information (sums of squares columns, intercept row, etc.) in the ANOVA table.
}
\item{return_aov}{
Logical. If TRUE, computes and returns an aov object corresponding to the requested ANOVA (useful for computing post-hoc contrasts).
}
}
\details{
ANCOVA is implemented by first regressing the DV against each covariate (after collapsing the data to the means of that covariate's levels per subject) and subtracting from the raw data the fitted values from this regression (then adding back the mean to maintain scale). These regressions are computed across Ss in the case of between-Ss covariates and computed within each Ss in the case of within-Ss covariates.
}
\section{Warning }{
Prior to running (though after obtaining running ANCOVA regressions as described in the \code{details} section), \code{dv} is collapsed to a mean for each cell defined by the combination of \code{wid} and any variables supplied to \code{within} and/or \code{between} and/or \code{diff}. Users are warned that while convenient when used properly, this automatic collapsing can lead to inconsistencies if the pre-collapsed data are unbalanced (with respect to cells in the full design) and only the partial design is supplied to \code{ezANOVA}. When this is the case, use \code{within_full} to specify the full design to ensure proper automatic collapsing.
}
\value{
A list containing one or more of the following components:
\item{ANOVA}{A data frame containing the ANOVA results.}
\item{Mauchly's Test for Sphericity}{If any within-Ss variables with >2 levels are present, a data frame containing the results of Mauchly's test for Sphericity. Only reported for effects >2 levels because sphericity necessarily holds for effects with only 2 levels.}
\item{Sphericity Corrections}{If any within-Ss variables are present, a data frame containing the Greenhouse-Geisser and Huynh-Feldt epsilon values, and corresponding corrected p-values.}
\item{Levene's Test for Homogeneity}{If the design is purely between-Ss, a data frame containing the results of Levene's test for Homogeneity of variance. Note that Huynh-Feldt corrected p-values where the Huynh-Feldt epsilon >1 will use 1 as the correction epsilon.}
\item{aov}{An aov object corresponding to the requested ANOVA.}
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
}
}
\author{
Michael A. Lawrence \email{mike.lwrnc@gmail.com}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
}
\seealso{
\code{\link{ezBoot}}, \code{\link{ezMixed}}, \code{\link{ezPerm}}, \code{\link{ezPlot}}, \code{\link{ezStats}}
}
\examples{

#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)
ezPrecis(ANT)


#Run an ANOVA on the mean correct RT data.
rt_anova = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = rt
    , wid = subnum
    , within = .(cue,flank)
    , between = group
)

#Show the ANOVA and assumption tests.
print(rt_anova)

\dontrun{
#Run an ANOVA on the mean correct RT data, ignoring group.
rt_anova2 = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = rt
    , wid = subnum
    , within = .(cue,flank)
)

#Show the ANOVA and assumption tests.
print(rt_anova2)
}

#Run a purely between-Ss ANOVA on the mean_rt data.
#NOTE use of within_full to ensure that the data are
# collapsed properly
rt_anova3 = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = rt
    , wid = subnum
    , within_full = .(cue,flank)
    , between = group
)

#Show the ANOVA and assumption tests.
print(rt_anova3)


#add a within-Ss effect to be used as a covariate
ANT$rt2 = ANT$rt + ANT$block*1000 #additive with and independent of the other predictors!

\dontrun{
#Run an anova that doesn't use the covariate
rt_anova4a = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = rt2
    , wid = subnum
    , within = .(cue,flank)
    , between = group
)

#Show the ANOVA and assumption tests.
# Note loss of power to observe the within effects
print(rt_anova4a)
}

#Run an anova that does use the covariate
rt_anova4b = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = rt2
    , wid = subnum
    , within = .(cue,flank)
    , within_covariates = block
    , between = group
)

#Show the ANOVA and assumption tests.
# Note power to observe the within effects has returned
print(rt_anova4b)


#add a between-Ss effect to be used as a covariate
ANT$bc = as.numeric(as.character(ANT$subnum))\%\%10 #Note that the effect is balanced across groups
ANT$rt3 = ANT$rt + ANT$bc*1000 #additive with and independent of the other predictors!

\dontrun{
#Run an anova that doesn't use the covariate
rt_anova5a = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = rt2
    , wid = subnum
    , within = .(cue,flank)
    , between = group
)

#Show the ANOVA and assumption tests.
# Note loss of power to observe the between effects
print(rt_anova5a)
}

#Run an anova that does use the covariate
rt_anova5b = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = rt2
    , wid = subnum
    , within = .(cue,flank)
    , between = group
    , between_covariates = bc
)

#Show the ANOVA and assumption tests.
# Note power to observe the between effects has returned
print(rt_anova5b)

}
