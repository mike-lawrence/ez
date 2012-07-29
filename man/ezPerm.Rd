\name{ezPerm}
\alias{ezPerm}
\title{Perform a factorial permutation test}
\description{
This function provides easy non-parametric permutation test analysis of data from factorial experiments, including purely within-Ss designs (a.k.a. \dQuote{repeated measures}), purely between-Ss designs, and mixed within-and-between-Ss designs.
}
\usage{
ezPerm(
data
, dv
, wid
, within = NULL
, between = NULL
, perms = 1e3
, parallel = FALSE
, alarm = FALSE
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
\item{between}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) between-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{perms}{
An integer \code{>} 0 specifying the number of permutations to compute.
}
\item{parallel}{
Logical. If TRUE, computation will be parallel, assuming that a parallel backend has been specified (as in \code{library(doMC);options(cores=4);registerDoMC()}. Likely only to work when running R from a unix terminal.)
}
\item{alarm}{
Logical. If TRUE, call the \code{\link[utils]{alarm}} function when \code{\link{ezPerm}} completes.
}
}
\details{
While \code{within} and \code{between} are both optional, at least one column of \code{data} must be provided to either \code{within} or \code{between}. Any numeric or character variables in \code{data} that are specified as either \code{wid}, \code{within} or \code{between} will be converted to a factor with a warning. The expected standard deviation of p-values is approximately \code{sqrt(true_p*(1-true_p)/perms)}; significance tests using an alpha of .05 should therefore employ at least 1e3 permutations. As the permutation test is computationally intensive, it is advisable to pre-test smaller values of \code{perms} and extrapolate to estimate the total test duration before attempting a full run. To facilitate such extrapolation, test duration is provided in the output after running a permutation test.
}
\value{
A data frame containing the permutation test results.
}
\section{Warning }{
\code{ezPerm()} is a work in progress. Under the current implementation, only main effects may be trusted.
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}
To report bugs or request features, please visit: \url{https://github.com/mike-lawrence/ez/issues}
To keep up to date on developments related to this package, join the discussion group at: \url{http://groups.google.com/group/ez4r}
}
\seealso{
\code{link{ezANOVA}}, \code{\link{ezBoot}}, \code{\link{ezMixed}}
}
\examples{
#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)
ezPrecis(ANT)

#Compute some useful statistics per cell.
cell_stats = ddply(
.data = ANT
, .variables = .( subnum , group , cue , flank )
, .fun = function(x){
#Compute error rate as percent.
error_rate = mean(x$error)*100
#Compute mean RT (only accurate trials).
mean_rt = mean(x$rt[x$error==0])
#Compute SD RT (only accurate trials).
sd_rt = sd(x$rt[x$error==0])
		to_return = data.frame(
			error_rate = error_rate
			, mean_rt = mean_rt
			, sd_rt = sd_rt
		)
return(to_return)
}
)

#Compute the grand mean RT per Ss.
gmrt = ddply(
.data = cell_stats
, .variables = .( subnum , group )
, .fun = function(x){
to_return = data.frame(
			mrt = mean(x$mean_rt)
		)
return(to_return)
}
)

#Run a purely between-Ss permutation test on the mean_rt data. 
mean_rt_perm = ezPerm(
data = gmrt
, dv = mrt
, wid = subnum
, between = group
, perms = 1e1 #1e3 or higher is best for publication
)

#Show the Permutation test.
print(mean_rt_perm)
}
