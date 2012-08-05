ezStats <-
function (
	data
	, dv
	, wid
	, within = NULL
	, within_full = NULL
	, within_covariates = NULL
	, between = NULL
	, between_full = NULL
	, between_covariates = NULL
	, diff = NULL
	, reverse_diff = FALSE
	, type = 2
	, check_args = TRUE
){
	if(check_args){
		args_to_check = c('dv','wid','within','between','within_full','between_full','diff','within_covariates','between_covariates')
		args = as.list(match.call()[-1])
		for(i in 1:length(args)){
			arg_name = names(args)[i]
			if(arg_name%in%args_to_check){
				if(is.symbol(args[[i]])){
					code = paste(arg_name,'=.(',as.character(args[[i]]),')',sep='')
					eval(parse(text=code))
				}else{
					if(is.language(args[[i]])){
						arg_vals = as.character(args[[i]])
						arg_vals = arg_vals[2:length(arg_vals)]
						arg_vals = paste(arg_vals,collapse=',')
						code = paste(arg_name,'=.(',arg_vals,')',sep='')
						eval(parse(text=code))
					}
				}
			}
		}
	}
	#get information for FLSD
	if(is.null(between_full)){
		temp_between = between
	}else{
		temp_between = between_full
	}
	from_ezANOVA_main = ezANOVA_main(
		data = data
		, dv = dv
		, wid = wid
		, within = within
		, within_full = within_full
		, within_covariates = within_covariates
		, between = temp_between
		, between_covariates = between_covariates
		, diff = diff
		, reverse_diff = reverse_diff
		, type = type
		, white.adjust = FALSE
		, return_aov = FALSE
	)
	this_ANOVA = from_ezANOVA_main$ANOVA
	data = from_ezANOVA_main$data
	#warn about FLSD for mixed
	if(!is.null(within) & !is.null(between)){
		if(!is.logical(diff) & length(within)>1){
			warning('Mixed within-and-between-Ss effect requested; FLSD is only appropriate for within-Ss comparisons (see warning in ?ezStats or ?ezPlot).',call.=FALSE)
		}
	}
	#compute N
	vars = as.character(c(between,within))
	temp = idata.frame(cbind(
		data
		, ezWID = data[,names(data) == as.character(wid)]
		, dummy = rep(1,length(data[,1]))
	))
	N = ddply(
		temp
		,structure(as.list(c(as.symbol('dummy'),between)),class = 'quoted')
		,function(x){
			to_return = length(unique(x$ezWID))
			names(to_return) = 'N'
			return(to_return)
		}
	)
	if(!all(N[,length(N)]==N[1,length(N)])){
		warning('Unbalanced groups. Mean N will be used in computation of FLSD')
		N = mean(N[,length(N)])
	}else{
		N = N[1,length(N)]
	}
	#compute FLSD
	DFd = this_ANOVA$DFd[length(this_ANOVA$DFd)]
	MSd = this_ANOVA$SSd[length(this_ANOVA$SSd)]/DFd
	Tcrit = qt(0.975,DFd)
	CI = Tcrit * sqrt(MSd/N)
	FLSD = sqrt(2) * CI
	#obtain stats
	temp = idata.frame(cbind(
		data
		, ezDV = data[,names(data) == as.character(dv)]
	))
	data <- ddply(
		temp
		,structure(as.list(c(between,within)),class = 'quoted')
		,function(x){
			N = length(x$ezDV)
			Mean = mean(x$ezDV)
			SD = sd(x$ezDV)
			return(c(N = N, Mean = Mean, SD = SD))
		}
	)
	data$FLSD = FLSD
	return(data)
}

