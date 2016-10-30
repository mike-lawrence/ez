\name{ezPredict}
\alias{ezPredict}
\title{Compute predicted values from the fixed effects of a mixed effects model}
\description{
This function computes the predicted values from the fixed effects of a mixed effects model.
}
\usage{
ezPredict(
    fit
    , to_predict = NULL
    , numeric_res = 0
    , boot = TRUE
    , iterations = 1e3
    , zero_intercept_variance = FALSE
)
}
\arguments{
\item{fit}{
Fitted \code{\link[lme4]{lmer}} object.
}
\item{to_predict}{
Optional data frame containing the fixed effects design to predict. If absent, the function will assume that the full design from the provided fitted model is requested.
}
\item{numeric_res}{
Integer value specifying the sampling resolution of any numeric fixed effect. Has no effect if non-NULL value supplied to \code{to_predict}. If \code{to_predict} is null and a numeric fixed effect is encountered in the fitted model, then predictions will be obtained at this many evenly spaced intervals between the minimum and maximum values in the original fitted data. The default value, 0, obtains predictions for each unique value found in the original data frame.
}
\item{boot}{
Logical. If TRUE (default), bootstrapping will be used to generate sample predictions.
}
\item{iterations}{
Integer value specifying the number of bootstrap iterations to employ if \code{boot==TRUE}.
}
\item{zero_intercept_variance}{
Logical. If TRUE (default), bootstrap samples will be obtained after setting the intercept variance and covariances to zero. This makes sense only when, prior to fitting the model, the predictor variables were set up with contrasts that make the intercept orthogonal to effects of interest (e.g. \code{contr.sum} or \code{contr.helmert}). This is useful to visualize cell means with confidence intervals that (roughly) can speak to differences between cells. However, it can be the case that even after zeroing the intercept variance, the resultant CIs on the raw cell values are deceptively large for comparing cells. So, if there appears to be no difference between cells, it is best to nonetheless re-visualize after collapsing the pair of cells of interest to a difference score (using the \code{diff} argument to \code{\link{ezPlot2}}).
}
}
\value{
A data frame containing the prediction value (and estimated variance of this value) for each cell in the fixed effects design.
}
\author{
Michael A. Lawrence \email{mike.lwrnc@gmail.com}\cr
Visit the \code{ez} development site at \url{http://github.com/mike-lawrence/ez}\cr
for the bug/issue tracker and the link to the mailing list.
}
\seealso{
\code{\link{ezMixed}}, \code{\link{ezPlot2}}
}
\examples{
library(lme4)

#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)

#fit a mixed effects model to the rt data
rt_fit = lmer(
    formula = rt ~ cue*flank*group + (1|subnum)
    , data = ANT[ANT$error==0,]
)

#obtain the predictions from the model
rt_preds = ezPredict(
    fit = rt_fit
)


#visualize the predictions
ezPlot2(
    preds = rt_preds
    , x = flank
    , row = cue
    , col = group
    , y_lab = 'RT (ms)'
)

}
