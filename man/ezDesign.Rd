\name{ezDesign}
\Rdversion{1.1}
\alias{ezDesign}
\title{
Function to plot the balance of data in an experimental design
}
\description{
This function provides easy visualization of the balance of data in a data set given a specified experimental design. This function is useful for identifying missing data and other issues (see examples).
}
\usage{
ezDesign(
    data
    , x
    , y
    , row = NULL
    , col = NULL
    , cell_border_size = 10
)
}
\arguments{
  \item{data}{
Data frame containing the data to be visualized.
}
  \item{x}{
.() object specifying the column in \code{data} that contains the variable to plot on the x-axis.
}
  \item{y}{
.() object specifying the column in \code{data} that contains the variable to plot on the y-axis.
}
  \item{row}{
Optional .() object specifying a variable by which to split the data into rows.
}
  \item{col}{
Optional .() object specifying a variable by which to split the data into columns.
}
  \item{cell_border_size}{
Numeric value specifying the size of the border seperating cells (0 specifies no border)
}
}
\details{
The function works by counting the number of rows in \code{data} in each cell of the design specified by the factorial combination of \code{x}, \code{y}, \code{row}, \code{col} variables.
}
\value{
A printable/modifiable ggplot2 object.
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
#Read in the ANT2 data (see ?ANT2).
data(ANT2)
head(ANT2)
ezPrecis(ANT2)

ezDesign(
    data = ANT2
    , x = .(trial)
    , y = .(subnum)
    , row = .(block)
    , col = .(group)
)
#subnum #7 is missing data from the last half of the experiment

ezDesign(
    data = ANT2
    , x = .(flank)
    , y = .(subnum)
    , row = .(cue)
)
#again, subnum#7 has half the data as the rest

#now look at error rates, which affect the number of RTs we can use
ezDesign(
    data = ANT2[ANT2$error==0,]
    , x = .(flank)
    , y = .(subnum)
    , row = .(cue)
)
#again, subnum#7 stands out because they have half the data as the rest
#also, subnum#14 has no data in any incongruent cells, suggesting that 
##they made all errors in this condition
#finally, subnum#12 has virtually no data, suggesting that they mistakenly 
##swapped responses

}
