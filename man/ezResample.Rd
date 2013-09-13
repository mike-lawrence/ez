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
    , resample_between = TRUE
    , check_args = TRUE
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
\item{resample_between}{
Logical. If TRUE (default), levels of \code{wid} are resampled.
}
\item{check_args}{
Users should leave this as its default (TRUE) value. This argument is intended for internal use only.
}
}
\value{
A data frame consisting of the resampled data
}
\author{
Michael A. Lawrence \email{mike.lwrnc@gmail.com}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
}
\seealso{
\code{\link{ezBoot}}
}
\examples{
library(plyr)
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
            , wid = .(subnum)
            , within = .(cue,flank)
            , between = .(group)
        )
        cell_vars = ddply(
            .data = idata.frame(this_resample)
            , .variables = .(subnum,cue,flank,group)
            , .fun = function(x){
                to_return = data.frame(
                    value = var(x$rt)
                )
                return(to_return)
            }
        )
        mean_cell_vars = ddply(
            .data = idata.frame(cell_vars)
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
    , .progress = 'time'
)



}
