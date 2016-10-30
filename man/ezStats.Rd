\name{ezStats}
\alias{ezStats}
\title{Compute descriptive statistics from a factorial experiment}
\description{
This function provides easy computation of descriptive statistics (between-Ss means, between-Ss SD, Fisher's Least Significant Difference) for data from factorial experiments, including purely within-Ss designs (a.k.a. \dQuote{repeated measures}), purely between-Ss designs, and mixed within-and-between-Ss designs.
}
\usage{
ezStats(
    data
    , dv
    , wid
    , within = NULL
    , within_full = NULL
    , within_covariates = NULL
    , between = NULL
    , between_full = NULL
    , between_covariates = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 2
    , check_args = TRUE
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
Name of the column in \code{data} that contains the variable specifying the case/Ss identifier.
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
\item{between_full}{
Same as \code{between}, but must specify the full set of between-Ss variables if \code{between} specifies only a subset of the design.
}
\item{between_covariates}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) between-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
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
\item{check_args}{
Users should leave this as its default (TRUE) value. This argument is intended for internal use only.
}
}
\details{
ANCOVA is implemented by first regressing the DV against each covariate (after collapsing the data to the means of that covariate's levels per subject) and subtracting from the raw data the fitted values from this regression (then adding back the mean to maintain scale). These regressions are computed across Ss in the case of between-Ss covariates and computed within each Ss in the case of within-Ss covariates.

Fisher's Least Significant Difference is computed as sqrt(2)*qt(.975,DFd)*sqrt(MSd/N), where N is taken as the mean N per group in cases of unbalanced designs.
}
\section{Warnings}{
Prior to running (though after obtaining running ANCOVA regressions as described in the \code{details} section), \code{dv} is collapsed to a mean for each cell defined by the combination of \code{wid} and any variables supplied to \code{within} and/or \code{between} and/or \code{diff}. Users are warned that while convenient when used properly, this automatic collapsing can lead to inconsistencies if the pre-collapsed data are unbalanced (with respect to cells in the full design) and only the partial design is supplied to \code{ezANOVA}. When this is the case, use \code{within_full} to specify the full design to ensure proper automatic collapsing.

The descriptives include Fisher's Least Significant Difference for the plotted effect, facilitating visual post-hoc multiple comparisons. To obtain accurate FLSDs when only a subset of the full between-Ss design is supplied to \code{between}, the full design must be supplied to \code{between_full}. Also note that in the context of mixed within-and-between-Ss designs, the computed FLSD values can only be used for within-Ss comparisons.
}
\value{
A data frame containing the descriptive statistics for the requested effect. N = number of Ss per cell. Mean = between-Ss mean. SD = between-Ss SD. FLSD = Fisher's Least Significant Difference.
}
\author{
Michael A. Lawrence \email{mike.lwrnc@gmail.com}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
}

\seealso{
\code{\link{ezANOVA}}, \code{\link{ezPlot}}
}
\examples{
#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)
ezPrecis(ANT)


#Run an ANOVA on the mean correct RT data.
mean_rt_anova = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = rt
    , wid = subnum
    , within = .(cue,flank)
    , between = group
)

#Show the ANOVA and assumption tests.
print(mean_rt_anova)

#Compute descriptives for the main effect of group.
group_descriptives = ezStats(
    data = ANT[ANT$error==0,]
    , dv = rt
    , wid = subnum
    , between = .(group)
)

#Show the descriptives.
print(group_descriptives)
}
