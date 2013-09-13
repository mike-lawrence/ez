ezBoot <-
function(
	data
	, dv
	, wid
	, within = NULL
	, between = NULL
	, resample_within = TRUE
	, iterations = 1e3
	, lmer = FALSE
	, lmer_family = gaussian
	, parallel = FALSE
	, alarm = FALSE
){
	args_to_check = c('dv','wid','within','between')
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
	start = proc.time()[3]
	vars = as.character(c(dv,wid,between,within))
	for(var in vars){
		if(!(var %in% names(data))){
			stop(paste('"',var,'" is not a variable in the data frame provided.',sep=''))			
		}
	}
	if(is.null(within) & is.null(between)){
		stop('is.null(within) & is.null(between)\nYou must specify at least one independent variable.')
	}
	if(!is.data.frame(data)){
		stop('"data" must be a data frame.')
	}
	if(!is.numeric(data[,names(data)==dv])){
		stop('"dv" must be numeric.')
	}
	if(!is.factor(data[,names(data)==wid])){
		warning(paste('Converting "',wid,'" to factor.',sep=''),call.=FALSE)
		data[,names(data)==wid]=factor(data[,names(data)==wid])
	}else{
		if(length(unique(data[,names(data)==wid]))!=length(levels(data[,names(data)==wid]))){
			warning(paste('You have removed one or more Ss from the analysis. Refactoring "',wid,'".',sep=''),call.=FALSE)
			data[,names(data)==wid]=factor(data[,names(data)==wid])
		}
	}
	vars = as.character(c(between,within))
	for(var in vars){
		if(!is.factor(data[,names(data)==var])){
			warning(paste('Converting "',var,'" to factor.',sep=''),call.=FALSE)
			data[,names(data)==var]=factor(data[,names(data)==var])
		}
		if(length(unique(data[,names(data)==var]))!=length(levels(data[,names(data)==var]))){
			warning(paste('You have removed one or more levels from variable "',var,'". Refactoring.',sep=''),call.=FALSE)
			data[,names(data)==var]=factor(data[,names(data)==var])
		}
		if(length(levels(data[,names(data)==var]))==1){
			stop(paste('"',var,'" has only one level."',sep=''))			
		}
	}
	names(data)[names(data)==as.character(dv)]='ezDV'
	if(resample_within){
		cell_size_per_id = ddply(
			.data = idata.frame(data)
			, .variables = structure(as.list(c(wid,between,within)),class = 'quoted')
			, .fun = function(x){
				to_return = data.frame(
					value = nrow(x)
				)
				return(to_return)
			}
		)
		if(all(cell_size_per_id$value<=1)){
			stop(paste('There are no within cells with multiple observations; please set the variable "resample_within" to FALSE.'))
		}
	}
	if(lmer){
		formula = paste(
			'ezDV~'
			, paste(vars,collapse='*')
			, '+(1|'
			, as.character(wid)
			, ')'
		)
		fit = lmer(
			formula = eval(parse(text=formula))
			, family = lmer_family
			, data = data
		)
		temp = list()
		j = 1
		for(i in structure(as.list(c(between,within)),class = 'quoted')){
			temp[[j]] = unique(data[,names(data)==as.character(i)])
			j = j + 1
		}
		cell_means = data.frame(expand.grid(temp))
		names(cell_means) = as.character(structure(as.list(c(between,within)),class = 'quoted'))
		cell_means$ezDV = 0
		mm = model.matrix(terms(fit),cell_means)
		value = mm %*% lme4::fixef(fit)
		cell_means$value = as.numeric(value[,1])
		cell_means = cell_means[,names(cell_means)!='ezDV']
	}else{
		cell_means_by_wid = ddply(
			.data = idata.frame(data)
			, .variables = structure(as.list(c(wid,between,within)),class = 'quoted')
			, .fun = function(x){
				to_return = data.frame(
					value = mean(x$ezDV)
				)
				return(to_return)
			}
		)
		cell_means = ddply(
			.data = idata.frame(cell_means_by_wid)
			, .variables = structure(as.list(c(between,within)),class = 'quoted')
			, .fun = function(x){
				to_return = data.frame(
					value = mean(x$value)
				)
				return(to_return)
			}
		)
	}
	boots = llply(
		.data = 1:iterations
		, .fun = function(x){
			done = FALSE
			while(!done){
				resampled_data = ezResample(data=data,wid=wid,within=within,between=between,resample_within=resample_within,check_args=F)
				if(lmer){
					fit = lmer(
						formula = eval(parse(text=formula))
						, family = lmer_family
						, data = resampled_data
					)
					value = mm %*% fixef(fit)
					cell_means$value = as.numeric(value[,1])
				}else{
					cell_means_by_wid = ddply(
						.data = idata.frame(resampled_data)
						, .variables = structure(as.list(c(wid,between,within)),class = 'quoted')
						, .fun = function(x){
							to_return = data.frame(
								value = mean(x$ezDV)
							)
							return(to_return)
						}
					)
					cell_means = ddply(
						.data = idata.frame(cell_means_by_wid)
						, .variables = structure(as.list(c(between,within)),class = 'quoted')
						, .fun = function(x){
							to_return = data.frame(
								value = mean(x$value)
							)
							return(to_return)
						}
					)
				}
				if(all(is.finite(cell_means$value))){
					done = TRUE
				}
			}
			cell_means$iteration = x
			return(cell_means)
		}
		, .progress = 'time'
		, .parallel = parallel
	)
	boots = Filter(Negate(empty), boots)
	boots = do.call(rbind,boots)	
	to_return = list()
	if(lmer){
		to_return$fit = fit
	}
	to_return$cells = cell_means
	to_return$boots = boots
	if(alarm){
		alarm()
	}
	return(to_return)
}

