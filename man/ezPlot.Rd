\name{ezPlot}
\alias{ezPlot}
\title{Plot data from a factorial experiment}
\description{
This function provides easy visualization of any given user-requested effect from factorial experiments, including purely within-Ss designs (a.k.a. \dQuote{repeated measures}), purely between-Ss designs, and mixed within-and-between-Ss designs. By default, Fisher's Least Significant Difference is computed to provide error bars that facilitate visual post-hoc multiple comparisons (see Warning section below).
}
\usage{
ezPlot(
data
, dv
, wid
, within = NULL
, between = NULL
, between_full = NULL
, x
, do_lines = TRUE
, do_bars = TRUE
, bar_width = NULL
, bar_size = NULL
, split = NULL
, row = NULL
, col = NULL
, to_numeric = NULL
, x_lab = NULL
, y_lab = NULL
, split_lab = NULL
, levels = NULL
, diff = NULL
, reverse_diff = FALSE
	, type = 2
, dv_levs = NULL
, dv_labs = NULL
, row_y_free = FALSE
)
}
\arguments{
\item{data}{
Data frame containing the data to be analyzed. OR, if multiple values are specified in \code{dv}, a list with as many element as values specified in \code{dv}, each element specifying a data frame for each \code{dv} in sequence.
}
\item{dv}{
.() object specifying the column in \code{data} that contains the dependent variable. Values in this column should be of the numeric class. Multiple values will yield a plot with dv mapped to row.
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
\item{x}{
.() object specifying the variable to plot on the x-axis.
}
\item{do_lines}{
Logical. If TRUE, lines will be plotted connecting groups of points.
}
\item{do_bars}{
Logical. If TRUE, error bars will be plotted.
}
\item{bar_width}{
Optional numeric value specifying custom widths for the error bar hat.
}
\item{bar_size}{
Optional numeric value or vector specifying custom size of the error bars.
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
Optional .() object specifying a 2-level within-Ss varbiable to collapse to a difference score.
}
\item{reverse_diff}{
Logical. If TRUE, triggers reversal of the difference collapse requested by \code{diff}.
}
\item{type}{Numeric value (either \code{1}, \code{2} or \code{3}) specifying the Sums of Squares \dQuote{type} to employ when data are unbalanced (eg. when group sizes differ). See \code{\link{ezANOVA}} for details.}
\item{dv_levs}{
Optional character vector specifying the factor ordering of multiple values specified in \code{dv}.
}
\item{dv_labs}{
Optional character vector specifying new factor labels for each of the multiple values specified in \code{dv}.
}
\item{row_y_free}{
Logical. If TRUE, then rows will permit different y-axis scales.
}
}
\details{
\begin{itemize}
\item While \code{within} and \code{between} are both optional, at least one column of \code{data} must be provided to either \code{within} or \code{between}. 
\item Fisher's Least Significant Difference is computed as sqrt(2)*qt(.975,DFd)*sqrt(MSd/N), where N is taken as the mean N per group in cases of unbalanced designs.
\end{itemize}
}
\section{Warnings }{
\begin{itemize}
\item Prior to running, \code{dv} is collapsed to a mean for each cell defined by the combination of \code{wid} and any variables supplied to \code{within} and/or \code{between} and/or \code{diff}. Users are warned that while convenient when used properly, this automatic collapsing can lead to inconsistencies if the pre-collapsed data are unbalanced (with respect to cells in the full design) and only the partial design is supplied to \code{ezPlot}. When in doubt, collapse your data by hand prior to using \code{ezPlot}.
\item The default error bars are Fisher's Least Significant Difference for the plotted effect, facilitating visual post-hoc multiple comparisons. Note however that in the context of mixed within-and-between-Ss designs, these bars can only be used for within-Ss comparisons.
\end{itemize}
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
\code{link{ezANOVA}}, \code{\link{ezStats}}
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

#Show the ANOVA and assumption tests.
print(mean_rt_anova)


#Plot the main effect of group.
group_plot = ezPlot(
data = ANT[ANT$error==0,]
, dv = .(rt)
, wid = .(subnum)
, between = .(group)
, x = .(group)
, do_lines = FALSE
, x_lab = 'Group'
, y_lab = 'RT (ms)'
)

#Show the plot.
print(group_plot)


#Re-plot the main effect of group, using the levels
##argument to re-arrange/rename levels of group
group_plot = ezPlot(
data = ANT[ANT$error==0,]
, dv = .(rt)
, wid = .(subnum)
, between = .(group)
, x = .(group)
, do_lines = FALSE
, x_lab = 'Group'
, y_lab = 'RT (ms)'
, levels = list(
group = list(
new_order = c('Treatment','Control')
, new_names = c('Treatment\nGroup','Control\nGroup')
)
)
)

#Show the plot.
print(group_plot)


#Plot the cue*flank interaction.
cue_by_flank_plot = ezPlot(
data = ANT[ANT$error==0,]
, dv = .(rt)
, wid = .(subnum)
, within = .(cue,flank)
, x = .(flank)
, split = .(cue)
, x_lab = 'Flanker'
, y_lab = 'RT (ms)'
, split_lab = 'Cue'
)

#Show the plot.
print(cue_by_flank_plot)


#Plot the cue*flank interaction by collapsing the cue effect to 
##the difference between None and Double
cue_by_flank_plot2 = ezPlot(
data = ANT[ ANT$error==0 & (ANT$cue \%in\% c('None','Double')) ,]
, dv = .(rt)
, wid = .(subnum)
, within = .(flank)
, diff = .(cue)
, reverse_diff = TRUE
, x = .(flank)
, x_lab = 'Flanker'
, y_lab = 'RT Effect (None - Double, ms)'
)

#Show the plot.
print(cue_by_flank_plot2)



#Plot the group*cue*flank interaction.
group_by_cue_by_flank_plot = ezPlot(
data = ANT[ANT$error==0,]
, dv = .(rt)
, wid = .(subnum)
, within = .(cue,flank)
, between = .(group)
, x = .(flank)
, split = .(cue)
, col = .(group)
, x_lab = 'Flanker'
, y_lab = 'RT (ms)'
, split_lab = 'Cue'
)

#Show the plot.
print(group_by_cue_by_flank_plot)


#Plot the group*cue*flank interaction in both error rate and mean RT.
group_by_cue_by_flank_plot_both = ezPlot(
data = list(
ANT
, ANT[ANT$error==0,]
)
, dv = .(error,rt)
, wid = .(subnum)
, within = .(cue,flank)
, between = .(group)
, x = .(flank)
, split = .(cue)
, col = .(group)
, x_lab = 'Flanker'
, split_lab = 'Cue'
, dv_labs = c('ER (\%)', 'RT (ms)')
, row_y_free = TRUE
)

#Show the plot.
print(group_by_cue_by_flank_plot_both)


}
