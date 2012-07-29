\name{ezResample}
\alias{ezResample}
\title{Resample data from a factorial experiment}
\description{
This function resamples data (useful when bootstrapping and used by \code{\link{ezBoot}}).
}
\usage{
ezResample(
	data
	, wid
	, within = NULL
	, between = NULL
	, resample_within = FALSE
)
}
\arguments{
\item{data}{
Data frame containing the data to be analyzed.
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
\item{resample_within}{
Logical. If TRUE, and if there are multiple observations per subject within each cell of the design specified by the factorial combination of variables supplied to \code{within} and \code{between}, then these observations-within-cells are resampled with replacement.
}

}
\value{
A data frame consisting of the resampled data
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}
To report bugs or request features, please visit: \url{https://github.com/mike-lawrence/ez/issues}
To keep up to date on developments related to this package, join the discussion group at: \url{http://groups.google.com/group/ez4r}
}
\seealso{
\code{\link{ezBoot}}
}
\examples{
#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)
ezPrecis(ANT)


#Bootstrap the within-cell variances
var_boots = ldply(
	.data = 1:1e1 #1e3 or higher should be used for publication
	, .fun = function(x){
		this_resample = ezResample(
			data = ANT[ANT$error==0,]
			, dv = .(rt)
		 , wid = .(subnum)
		 , within = .(cue,flank)
		 , between = .(group)
		)
		cell_vars = ddply(
			.data = this_resample
			, .variables = .(subnum,cue,flank,group)
			, .fun = function(x){
				to_return = data.frame(
					value = var(x$rt)
				)
				return(to_return)
			}
		)
		mean_cell_vars = ddply(
			.data = cell_vars
			, .variables = .(cue,flank,group)
			, .fun = function(x){
				to_return = data.frame(
					value = mean(x$value)
				)
				return(to_return)
			}
		)
		mean_cell_vars$iteration = x
		return(mean_cell_vars)
	}
	, .progress = 'timeCI'
)



}
