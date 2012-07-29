ezPerm <-
function(
	data
	, dv
	, wid
	, within = NULL
	, between = NULL
	, perms = 1e3
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
	if(is.null(within) & is.null(between)){
		stop('is.null(within) & is.null(between)\nYou must specify at least one independent variable.')
	}
	if(!is.data.frame(data)){
		stop('"data" must be a data frame.')
	}
	if(!is.numeric(data[,names(data)==dv])){
		stop('"dv" must be numeric.')
	}
	if(!is.numeric(perms)){
		stop('"perms" must be numeric.')
	}else{
		if(perms<0){
			stop('"perms" must be >= 0.')			
		}else{
			if(perms%%1){
				stop('"perms" must be an integer.')
			}
		}
	}
	if(!is.factor(data[,names(data)==wid])){
		warning(paste('Converting "',wid,'" to factor for ANOVA.',sep=''),call.=FALSE)
		data[,names(data)==wid]=factor(data[,names(data)==wid])
	}
	for(i in within){
		if(!is.factor(data[,names(data)==i])){
			warning(paste('Converting "',i,'" to factor for ANOVA.',sep=''),call.=FALSE)
			data[,names(data)==i]=factor(data[,names(data)==i])
		}
	}
	for(i in between){
		if(!is.factor(data[,names(data)==i])){
			warning(paste('Converting "',i,'" to factor for ANOVA.',sep=''),call.=FALSE)
			data[,names(data)==i]=factor(data[,names(data)==i])
		}
	}
	data=data[,names(data) %in% c(within,between,wid,dv)]
	aov_formula = paste(
		as.character(dv)
		,'~'
		,paste(as.character(between),collapse = '*')
		,ifelse(is.null(between),'',ifelse(is.null(within),'','*'))
		,paste(as.character(within),collapse = '*')
		,ifelse(
			is.null(within)
			,paste(
				'+Error('
				,as.character(wid)
				,')'
				,sep = ''
			)
			,paste(
				'+Error('
				,as.character(wid)
				,'/('
				,paste(as.character(within),collapse = '*')
				,'))'
				,sep = ''
			)
		)
		,sep = ''
	)	
	obs = ezPerm_aov(data,aov_formula)
	sim_data=data
	if(!is.null(between)){
		group_info = ddply(
			sim_data
			,.variables = structure(as.list(c(wid,between)),class='quoted')
			,function(x){
				x[1,]
			}
		)
	}
	sim = laply(
		.data = 1:perms
		, .fun = function(this_perm){
			set.seed(this_perm)
			for(this_within in within){
				sim_data = ddply(
					sim_data
					,.variables = structure(as.list(c(wid,structure(within[within!=this_within],class='quoted'))),class='quoted')
					,function(x){
						x[,names(x)==this_within] = sample(x[,names(x)==this_within])
						return(x)
					}
				)
			}
			for(this_between in between){
				group_info[,names(group_info)==this_between]=sample(group_info[,names(group_info)==this_between])
				for(this_wid in sim_data[,names(sim_data)==wid]){
					sim_data[sim_data[,names(sim_data)==wid]==this_wid,names(sim_data)==this_between] = group_info[group_info[,names(group_info)==wid]==this_wid,names(group_info)==this_between]
				}
			}
			return(ezPerm_aov(sim_data,aov_formula))
		}
		, .progress = 'time'
		, .parallel = parallel
	)
	sim = matrix(sim,ncol=perms)
	from_terms = terms(eval(parse(text=aov_formula)))
	term_labels = attr(from_terms,'term.labels')
	term_labels = term_labels[grep('(Intercept)',term_labels,fixed=T,invert=T)]
	term_labels = term_labels[grep('Error(',term_labels,fixed=T,invert=T)]
	perm_test = data.frame(Effect=term_labels)
	perm_test$p = rowMeans(sim>=obs)
	perm_test$'p<.05' = ifelse(perm_test$p<.05,'*','')
	if(alarm){
		alarm()
	}
	return(perm_test)
}

