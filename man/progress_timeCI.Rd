\name{progress_timeCI}
\alias{progress_timeCI}
\title{Progress bar with time remaining estimation including confidence intervals}
\description{
This function is intended to be used with the various plyr functions. Bootstrap resampling obtains 10%ile, 50%ile and 90%ile estimates for time remaining (useful when each iteration of the plyr function takes a varying amount of time). See example below.
}
\usage{
progress_timeCI()
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}
To report bugs or request features, please visit: \url{https://github.com/mike-lawrence/ez/issues}
To keep up to date on developments related to this package, join the discussion group at: \url{http://groups.google.com/group/ez4r}
}
\examples{

\dontrun{
l_ply(
	.data = 1:1e3
	, .fun = function(x){
		Sys.sleep(rexp(1,1))
	}
	, .progress = 'timeCI'
)
}

}
