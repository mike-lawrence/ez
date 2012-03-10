\name{ezBootPlot}
\Rdversion{1.1}
\alias{ezBootPlot}
\title{
Function to plot bootstrapped predictions and confidence intervals.
}
\description{
This function provides easy visualization of any given user-requested effect from the bootstrap predictions computed by \code{\link{ezBoot}}.
}
\usage{
ezBootPlot(
    from_ezBoot
    , confidence = .95
    , x
    , split = NULL
    , row = NULL
    , col = NULL
    , do_lines = TRUE
    , bar_width = NULL
    , to_numeric = NULL
    , x_lab = NULL
    , y_lab = NULL
    , split_lab = NULL
    , levels = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , row_y_free = FALSE
    , alarm = TRUE
)
}
\arguments{
  \item{from_ezBoot}{
An list object resulting from a call to \code{\link{ezBoot}}.
}
  \item{confidence}{
Numeric vector of one or more confidence levels to use for plotting error bars. If plotting multiple confidence regions, it is suggested that an equal number of different values are supplied to the \code{bar_width} argument for differentiation.
}
  \item{x}{
.() object specifying the variable to plot on the x-axis.
}
  \item{split}{
Optional .() object specifying a variable by which to split the data into different shapes/colors (and line types, if do_lines==TRUE).
}
  \item{row}{
Optional .() object specifying a variable by which to split the data into rows.
}
  \item{col}{
Optional .() object specifying a variable by which to split the data into columns.
}
  \item{do_lines}{
Logical. If TRUE, lines will be plotted connecting groups of points.
}
  \item{bar_width}{
Optional numeric value specifying custom widths for the error bar hat. Must either have a length of 1, or the same length as \code{confidence}.
}
  \item{to_numeric}{
Optional .() object specifying any variables that need to be converted to the numeric class before plotting.
}
  \item{x_lab}{
Optional character string specifying the x-axis label.
}
  \item{y_lab}{
Optional character string specifying the y-axis label.
}
  \item{split_lab}{
Optional character string specifying the key label.
}
  \item{levels}{
Optional named list where each item name matches a factored column in \code{data} that needs either reordering of levels, renaming of levels, or both. Each item should be a list containing named elements \code{new_order} or \code{new_names} or both.
}
  \item{diff}{
Optional .() object specifying a 2-level varbiable to collapse to a difference score.
}
  \item{reverse_diff}{
Logical. If TRUE, triggers reversal of the difference collapse requested by \code{diff}.
}
  \item{row_y_free}{
Logical. If TRUE, then rows will permit different y-axis scales.
}
  \item{alarm}{
Logical. If TRUE (default), call the \code{\link[utils]{alarm}} function when \code{\link{ezBootPlot}} completes.
}
}
\value{
A list with 4 components:
\item{plot}{A printable/modifiable ggplot2 object.}
\item{cells}{A data frame containing predictions for each cell in the requested design.}
\item{boots}{A data frame containing each iteration of bootstrap predictions, collased to the requested design.}
\item{boot_stats}{A data frame containing the "lo" and "hi" bounds of the 95 percent bootstrap confidence interval of each cell in the requested design.}
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
#see examples in ezBoot
}
