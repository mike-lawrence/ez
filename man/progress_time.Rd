\name{progress_time}
\alias{progress_time}
\title{Progress bar with time remaining estimation}
\description{
This function is intended to be used with the various plyr functions. See examples below.
}
\usage{
progress_time()
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
}
\examples{

\dontrun{
l_ply(
    .data = 1:1e3
    , .fun = function(x){
        Sys.sleep(1)
    }
    , .progress = 'time'
)
}

}
