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
    , within_full = NULL
    , within_covariates = NULL
    , between = NULL
    , between_full = NULL
    , between_covariates = NULL
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
    , y_free = FALSE
    , print_code = FALSE
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
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) within-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{within_full}{
Same as within, but intended to specify the full within-Ss design in cases where the data have not already been collapsed to means per condition specified by \code{within} and when \code{within} only specifies a subset of the full design.
}
\item{within_covariates}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) within-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{between}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) between-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
}
\item{between_full}{
Same as \code{between}, but must specify the full set of between-Ss variables if \code{between} specifies only a subset of the design.
}
\item{between_covariates}{
Names of columns in \code{data} that contain predictor variables that are manipulated (or observed) between-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
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
\item{y_free}{
Logical. If TRUE, then rows will permit different y-axis scales.
}
\item{print_code}{
Logical. If TRUE, the code for creating the ggplot2 plot object is printed and the data to be plotted is returned instead of the plot itself.
}
}
\details{
ANCOVA is implemented by first regressing the DV against each covariate (after collapsing the data to the means of that covariate's levels per subject) and subtracting from the raw data the fitted values from this regression (then adding back the mean to maintain scale). These regressions are computed across Ss in the case of between-Ss covariates and computed within each Ss in the case of within-Ss covariates.

Fisher's Least Significant Difference is computed as sqrt(2)*qt(.975,DFd)*sqrt(MSd/N), where N is taken as the mean N per group in cases of unbalanced designs.
}
\section{Warnings}{
Prior to running (though after obtaining running ANCOVA regressions as described in the \code{details} section), \code{dv} is collapsed to a mean for each cell defined by the combination of \code{wid} and any variables supplied to \code{within} and/or \code{between} and/or \code{diff}. Users are warned that while convenient when used properly, this automatic collapsing can lead to inconsistencies if the pre-collapsed data are unbalanced (with respect to cells in the full design) and only the partial design is supplied to \code{ezANOVA}. When this is the case, use \code{within_full} to specify the full design to ensure proper automatic collapsing.

The default error bars are Fisher's Least Significant Difference for the plotted effect, facilitating visual post-hoc multiple comparisons. To obtain accurate FLSDs when only a subset of the full between-Ss design is supplied to \code{between}, the full design must be supplied to \code{between_full}. Also note that in the context of mixed within-and-between-Ss designs, the computed FLSD bars can only be used for within-Ss comparisons.
}
\value{
If \code{print_code} is FALSE, printable/modifiable ggplot2 object is returned. If \code{print_code} is TRUE, the code for creating the ggplot2 plot object is printed and the data to be plotted is returned instead of the plot itself.
}
\author{
Michael A. Lawrence \email{Mike.Lawrence@dal.ca}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
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

#tweak the plot (if using ggplot2 version 0.9.1)
# group_plot = group_plot + 
# opts(
#     panel.grid.major = theme_blank()
#     , panel.grid.minor = theme_blank()
# )
# print(group_plot)

#tweak the plot (if using ggplot2 version 0.9.2)
# group_plot = group_plot + 
# theme(
#     panel.grid.major = element_blank()
#     , panel.grid.minor = element_blank()
# )
# print(group_plot)



#use the "print_code" argument to print the 
# code for creating the plot and return the
# data to plot. This is useful when you want
# to learn how to create plots from scratch
# (which can in turn be useful when you can't
# get a combination of ezPlot and tweaking to 
# achieve what you want) 
group_plot_data = ezPlot(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , wid = .(subnum)
    , between = .(group)
    , x = .(group)
    , do_lines = FALSE
    , x_lab = 'Group'
    , y_lab = 'RT (ms)'
    , print_code = TRUE
)


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
    , y_free = TRUE
)

#Show the plot.
print(group_by_cue_by_flank_plot_both)


}
