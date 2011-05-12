\name{ezStats}
\Rdversion{1.1}
\alias{ezStats}
\title{
Function to obtain descriptive statistics from a factorial experiment
}
\description{
This function provides easy computation of descriptive statistics (between-Ss means, between-Ss SD, Fisher's Least Significant Difference) for data from factorial experiments, including purely within-Ss designs (a.k.a. "repeated measures"), purely between-Ss designs, and mixed within-and-between-Ss designs. 
}
\usage{
ezStats(
    data
    , dv
    , wid
    , within = NULL
    , between = NULL
    , between_full = NULL
    , diff = NULL
    , reverse_diff = FALSE
	, type = 2
)
}
\arguments{
  \item{data}{
Data frame containing the data to be analyzed.
}
  \item{dv}{
.() object specifying the column in \code{data} that contains the dependent variable. Values in this column should be of the numeric class.
}
  \item{wid}{
.() object specifying the column in \code{data} that contains the variable specifying the case/Ss identifier. Values in this column will be converted to factor class if necessary.
}
  \item{within}{
Optional .() object specifying the column(s) in \code{data} that contains predictor variables that are manipulated within-Ss. Values in this column will be converted to factor class if necessary.
}
  \item{between}{
Optional .() object specifying the column(s) in \code{data} that contains predictor variables that are manipulated between-Ss. Values in this column will be converted to factor class if necessary.
}
  \item{between_full}{
Same as \code{between}, but must specify the full set of between-Ss variables if \code{between} specifies only a subset of the design.
}
  \item{diff}{
Optional .() object specifying a 2-level within-Ss varbiable to collapse to a difference score.
}
  \item{reverse_diff}{
Logical. If TRUE, triggers reversal of the difference collapse requested by \code{diff}.
}
  \item{type}{Numeric value (either \code{1}, \code{2} or \code{3}) specifying the Sums of Squares "type" to employ when data are unbalanced (eg. when group sizes differ). See \code{\link{ezANOVA}} for details.}
}
\details{
While \code{within} and \code{between} are both optional, at least one column of \code{data} must be provided to either \code{within} or \code{between}. Any numeric or character variables in \code{data} that are specified as either \code{wid}, \code{within} or \code{between} will be converted to a factor with a warning. Fisher's Least Significant Difference is computed as sqrt(2)*qt(.975,DFd)*sqrt(MSd/N), where N is taken as the mean N per group in cases of unbalanced designs.
}
\value{
A data frame containing the descriptive statistics for the requested effect. N = number of Ss per cell. Mean = between-Ss mean. SD = between-Ss SD. FLSD = Fisher's Least Significant Difference.}
\section{Warning }{
The descriptives include Fisher's Least Significant Difference for the requested effect. In the context of purely within-Ss or purely between-Ss this value may be used for post-hoc multiple comparisons. Note however that in the context of mixed within-and-between-Ss designs, this value can only be used for within-Ss comparisons.
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
mean_rt_anova = ezANOVA(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , wid = .(subnum)
    , within = .(cue,flank)
    , between = .(group)
)

#Show the ANOVA & assumption tests.
print(mean_rt_anova)

#Compute descriptives for the main effect of group.
group_descriptives = ezStats(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , wid = .(subnum)
    , between = .(group)
)

#Show the descriptives.
print(group_descriptives)
}
