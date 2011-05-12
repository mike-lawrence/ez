\name{ezPredict}
\Rdversion{1.1}
\alias{ezPredict}
\title{
Computes predicted values from the fixed effects of a mixed effects model
}
\description{
This function computes the predicted values from the fixed effects of a mixed effects model.
}
\usage{
ezPredict( 
	fit
	, to_predict = NULL
	, numeric_res = 0
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
}
\value{
A data frame containing the prediction value (and estimated variance of this value) for each cell in the fixed effects design.
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
#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)

#fit a mixed effects model to the error rate data
er_fit = lmer(
	formula = error ~ cue*flank*group + (1|subnum)
	, family = binomial
	, data = ANT
)

#obtain the predictions from the model
er_preds = ezPredict(
	fit = er_fit
)

#compute 95% CI for each prediction
er_preds$lo = er_preds$value - qnorm(.975)*sqrt(er_preds$var)
er_preds$hi = er_preds$value + qnorm(.975)*sqrt(er_preds$var)


#visualize the predictions
ggplot(
	data = er_preds
	, mapping = aes(
		x = flank
		, y = value
		, ymin = lo
		, ymax = hi
	)
)+
geom_point(
	alpha = .75
)+
geom_line(
	alpha = .5
)+
geom_errorbar(
	alpha = .5
)+
facet_grid(
	cue ~ group
)+
labs(
	y = 'Error Rate (log odds)'
)

}
