\name{ezPlot2}
\alias{ezPlot2}
\title{Plot bootstrap predictions and confidence intervals}
\description{
This function provides easy visualization of any given user-requested effect from the bootstrap predictions computed by \code{\link{ezPredict}} or \code{\link{ezBoot}}.
}
\usage{
ezPlot2(
	preds
	, confidence = .95
	, x = NULL
	, split = NULL
	, row = NULL
	, col = NULL
	, do_lines = TRUE
	, ribbon = FALSE
	, confidence_alpha = .5
	, point_alpha = .8
	, line_alpha = .8
	, bar_width = NULL
	, to_numeric = NULL
	, x_lab = NULL
	, y_lab = NULL
	, split_lab = NULL
	, levels = NULL
	, diff = NULL
	, reverse_diff = NULL
	, row_y_free = FALSE
	, alarm = FALSE
	, do_plot = TRUE
	, parallel = FALSE
)
}
\arguments{
\item{preds}{
An list object resulting from a call to \code{\link{ezPredict}} or \code{\link{ezBoot}}.
}
\item{confidence}{
Numeric vector of one or more confidence levels to use for plotting error bars. If plotting multiple confidence regions, it is suggested that an equal number of different values are supplied to the \code{bar_width} argument for differentiation.
}
\item{x}{
Name of the variable to plot on the x-axis.
}
\item{split}{
Name of a variable by which to split the data into different shapes/colors (and line types, if do_lines==TRUE).
}
\item{row}{
Name of a variable by which to split the data into facet rows.
}
\item{col}{
Name of a variable by which to split the data into facet columns.
}
\item{do_lines}{
Logical. If TRUE, lines will be plotted connecting groups of points.
}
\item{bar_width}{
Numeric value or vector specifying custom widths for the error bar hat. Must either have a length of 1, or the same length as \code{confidence}.
}
\item{to_numeric}{
Names of any variables that need to be converted to the numeric class before plotting. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{x_lab}{
Character string specifying the x-axis label.
}
\item{y_lab}{
Character string specifying the y-axis label.
}
\item{split_lab}{
Character string specifying the key label.
}
\item{levels}{
Named list where each item name matches a factored column in \code{data} that needs either reordering of levels, renaming of levels, or both. Each item should be a list containing named elements \code{new_order} or \code{new_names} or both.
}
\item{diff}{
Names of any variables to collapse to a difference score. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list. All supplied variables must be factors, ideally with only two levels (especially if setting the \code{reverse_diff} argument to TRUE).
}
\item{reverse_diff}{
Logical. If TRUE, triggers reversal of the difference collapse requested by \code{diff}. Take care with variables with more than 2 levels.
}
\item{row_y_free}{
Logical. If TRUE, then rows will permit different y-axis scales.
}
\item{alarm}{
Logical. If TRUE, call the \code{\link[utils]{alarm}} function when \code{\link{ezPlot2}} completes (useful for plots that take a long time).
}
\item{do_plot}{
Logical. If TRUE, no plot will be produced but instead a data frame containing point predictions and confidence limits will be returned.
}
\item{parallel}{
Logical. If TRUE, computation will be parallel, assuming that a parallel backend has been specified (as in \code{library(doMC);options(cores=4);registerDoMC()}. Likely only to work when running R from a unix terminal.)
}
}
\value{
If \code{do_plot} is TRUE (default), a printable/modifiable ggplot2 object representing the predictions and confidence intervals. If \code{do_plot} is FALSE, a data frame containing the predictions and confidence intervals.
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}
To report bugs or request features, please visit: \url{https://github.com/mike-lawrence/ez/issues}
To keep up to date on developments related to this package, join the discussion group at: \url{http://groups.google.com/group/ez4r}
}
\seealso{
\code{\link{ezBoot}}, \code{\link{ezMixed}}, \code{\link{ezPredict}}
}
\examples{

}
