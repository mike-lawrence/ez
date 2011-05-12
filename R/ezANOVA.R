ezANOVA <-
function(
	data
	, dv
	, wid
	, within = NULL
	, between = NULL
	, observed = NULL
	, diff = NULL
	, reverse_diff = FALSE
	, type = 2
	, white.adjust = FALSE
	, detailed = FALSE
	, return_aov = FALSE
){
	to_return = ezANOVA_main(
		data = data
		, dv = dv
		, wid = wid
		, within = within
		, between = between
		, observed = observed
		, diff = diff
		, reverse_diff = reverse_diff
		, type = type
		, return_aov = return_aov
		, white.adjust = white.adjust
	)	
	
	########
	# Compute effect size
	########
	if(!white.adjust){
		if(!is.null(observed)){
			obs = rep(F,nrow(to_return$ANOVA))
			for(i in as.character(observed)){
				obs = obs | str_detect(to_return$ANOVA$Effect,i)
			}
			obs_SSn1 = sum(to_return$ANOVA$SSn*obs)
			obs_SSn2 = to_return$ANOVA$SSn*obs
		}else{
			obs_SSn1 = 0
			obs_SSn2 = 0
		}
		to_return$ANOVA$ges = to_return$ANOVA$SSn/(to_return$ANOVA$SSn+sum(unique(to_return$ANOVA$SSd))+obs_SSn1-obs_SSn2)
	}

	########
	# Final clean-up
	########

	#remove the data & type label from to_return
	to_return = to_return[!(names(to_return) %in% c('data','type'))]

	#if necessary, remove extra columns and the Intercept row from the anova
	if(!detailed){
		to_return$ANOVA = to_return$ANOVA[,names(to_return$ANOVA) %in% c('Effect','DFn','DFd','F','p','p<.05','ges')]
		row_keep = !str_detect(to_return$ANOVA$Effect,'(Intercept)')
		to_return$ANOVA = to_return$ANOVA[row_keep,]
	}

	#all done!
	return(to_return)
}

