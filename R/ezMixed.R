ezMixed <-
function(
	data
	, dv
	, family = gaussian
	, random
	, fixed
	, covariates = NULL
	, add_q = FALSE
	, fix_gam = TRUE
	, cov_gam = TRUE
	, gam_smooth = c('s','te')
	, gam_bs = 'ts'
	, gam_k = Inf
	, use_bam = FALSE
	, alarm = FALSE
	, term_labels = NULL
	, highest = Inf
	, return_models = TRUE
	, correction = AIC
	, progress_dir = NULL
	, resume = FALSE
	, parallelism = 'none'
	, gam_args = NULL
	, mer_args = NULL
){
	args_to_check = c('dv','random','fixed','covariates')
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
	if(!is.null(progress_dir)){
		if(!file.exists(progress_dir)){
			dir.create(progress_dir)
		}
		if(!file.exists(paste(progress_dir,'models',sep='/'))){
			dir.create(paste(progress_dir,'models',sep='/'))
		}
		if(return_models){
			warning(paste('"progress_dir" set to "',progress_dir,'"; setting "return_models" to FALSE.',sep=''),immediate.=TRUE,call.=FALSE)
			return_models = FALSE
		}
		if(resume){
			terms_done = list.files(
				path = progress_dir
				, pattern = '.RData'
			)
		}
	}
	start = proc.time()[3]
	if(!is.data.frame(data)){
		stop('"data" must be a data frame.')
	}
	if(!is.numeric(data[,names(data)==dv])){
		stop('"dv" must be numeric.')
	}
	vars = as.character(c(dv,random,fixed,covariates))
	for(var in vars){
		if(!(var %in% names(data))){
			stop(paste('"',var,'" is not a variable in the data frame provided.',sep=''))			
		}
		if(is.character(data[,names(data)==var])){
			data[,names(data)==var] = factor(data[,names(data)==var])
			warning(paste('Converting "',var,'" from character to factor.',sep=''),immediate.=TRUE,call.=FALSE)
		}
	}
	numeric_covariates = NULL
	if(cov_gam){
		for(i in 1:length(covariates)){
			if(is.numeric(data[,names(data)==as.character(covariates[i])])){
				numeric_covariates = c(numeric_covariates,as.character(covariates[i]))
			}
		}		
	}
	numeric_fixed = NULL
	if(fix_gam){
		for(i in 1:length(fixed)){
			if(is.numeric(data[,names(data)==as.character(fixed[i])])){
				if(length(unique(data[,names(data)==as.character(fixed[i])]))>2){
					numeric_fixed = c(numeric_fixed,as.character(fixed[i]))
				}
			}else{
				data[,names(data)==as.character(fixed[i])] = ordered(data[,names(data)==as.character(fixed[i])])
			}
		}		
	}
	if(add_q){
		numeric_fixed = c(numeric_fixed,'q')
		fixed =  structure(as.list(c(fixed,.(q))),class = 'quoted')
	}
	if(is.null(term_labels)){
		to_terms = paste('y~',paste(fixed,collapse='*'))
		from_terms = terms(eval(parse(text=to_terms)))
		term_labels = attr(from_terms,'term.labels')
	}
	if(is.finite(highest)){
		term_labels = term_labels[laply((strsplit(term_labels,':')),length)<=highest]
	}
	for(i in 1:length(term_labels)){
		temp = unlist(strsplit(term_labels[i],':'))
		temp = temp[order(temp)]
		term_labels[i] = paste(temp,collapse=':')
	}
	term_labels = term_labels[order(str_count(term_labels,':'),term_labels)]
	if(add_q){
		term_labels = term_labels[term_labels!='q']
	}
	cat('  bits e w effect\n------ - - ------\n\r')
	process_term = function(this_term_num){
		term_text = str_replace_all(term_labels[this_term_num],':','BY')
		if(resume){
			if(paste(term_text,'.RData',sep='') %in% terms_done){
				eval(parse(text=paste("load(paste(progress_dir,'/",term_text,".RData',sep=''))",sep="")))
				bits = format(c(out_from_process_term$summary$bits,-1), digits=1, nsmall = 2,scientific=T)
				cat(
					c(
						bits[1]
						, ifelse(out_from_process_term$summary$error,'X','-')
						, ifelse(out_from_process_term$summary$warning,'X','-')
						, term_labels[this_term_num]
						, '\n\r'
					)
					, sep = ' '
				)
				flush.console()
				return(out_from_process_term)
			}
		}
		if(parallelism!='full'){
			cat(
				c(
					'      '
					, ' '
					, ' '
					, term_labels[this_term_num]
					, '\r'
				)
				, sep = ' '
			)
			flush.console()
		}
		effect = term_labels[this_term_num]
		effect_split = strsplit(effect,':')[[1]]
		this_height = length(effect_split)
		numeric_fixed_num = sum(effect_split%in%numeric_fixed)
		this_data = data
		for(i in effect_split[effect_split!='q']){
			this_data = this_data[!is.na(this_data[,names(this_data)==i]),]
		}
		if('q' %in% effect_split){
			this_data = ddply(
				.data = data
				, .variables = structure(as.list(c(random,fixed[(fixed%in%effect_split)&(fixed!='q')])),class = 'quoted')
				, .fun = function(x){
					to_return = data.frame(
						q = ((1:nrow(x))-.5)/nrow(x)
						, EZTEMP = sort(x[,names(x)==dv])
					)
					names(to_return)[ncol(to_return)] = dv
					return(to_return)
				}
			)
		}
		if((!is.null(numeric_covariates)&cov_gam)|(fix_gam&(numeric_fixed_num>0))){
			formula_base = paste(
				as.character(dv)
				, '~'
				, paste(
					's('
					, random
					, ',bs="re")'
					, collapse='+'
					, sep = ''
				)
				, '+'
			)
			if(!is.null(covariates)){
				if(!is.null(covariates[covariates %in% numeric_covariates])){
					formula_base = paste(
						formula_base
						, '+'
						, paste(
							gam_smooth[1]
							,'('
							, covariates[covariates %in% numeric_covariates]
							, ',bs="'
							, gam_bs
							, '")'
							, collapse = '+'
							, sep = ''
						)
						, '+'
						, sep = ''
					)
				}
				if(!is.null(covariates[!(covariates %in% numeric_covariates)])){
					formula_base = paste(
						formula_base
						, '+'
						, paste(
							covariates[!(covariates %in% numeric_covariates)]
							, collapse = '+'
						)
						, '+'
						, sep = ''
					)			
				}
			}
		}else{
			formula_base = paste(
				as.character(dv)
				, '~'
				, paste(
					'(1|'
					, random
					, ')'
					, collapse='+'
					, sep = ''
				)
				, '+'
			)
			if(is.null(covariates)){
				formula_base = paste(
					formula_base
					, '+'
					, paste(
						covariates
						, collapse = '+'
					)
					, '+'
					, sep = ''
				)
			}
		}
		if((fix_gam&(numeric_fixed_num>0))){
			if(this_height==1){
				restricted = 'NULL'
				k = min(
					gam_k
					, length(unique(this_data[,names(this_data)==effect]))
				)
				unrestricted = paste(
					gam_smooth[1]
					,'('
					, effect
					, ',k='
					, k
					, ',bs="'
					, gam_bs
					, '")'
					, sep=''
				)
			}else{
				convert_to_gam_formula = function(formula){
					temp = terms(eval(parse(text=formula)))
					temp = attr(temp,'term.labels')
					for(i in 1:length(temp)){
						temp_split = strsplit(temp[i],':')[[1]]
						temp_numeric = temp_split[temp_split%in%numeric_fixed]
						if(length(temp_numeric)!=0){
							k = rep(NA,length(temp_numeric))
							for(j in 1:length(temp_numeric)){
								k[j] = min(c(gam_k,length(unique(this_data[,names(this_data)==temp_numeric[j]]))))
							}
							temp_not_numeric = temp_split[!(temp_split%in%numeric_fixed)]
							if(length(temp_not_numeric)==0){
								temp[i] = paste(
									ifelse(numeric_fixed_num>1,ifelse(length(gam_smooth)>1,gam_smooth[2],gam_smooth),gam_smooth[1])
									,'('
									, paste(temp_numeric,collapse=',')
									, ',k=c('
									, paste(k,collapse=',')
									, '),bs="'
									, gam_bs
									, '")'
									, sep=''
								)
							}else{
								dummy = paste(temp_not_numeric,collapse='BY')
								#dummy = paste(temp_not_numeric,'ORDERED',collapse='BY',sep='')
								if(!(dummy%in%names(this_data))){
									this_data$ezDUMMY <<- ''
									for(this_temp_not_numeric in temp_not_numeric){
										this_data$ezDUMMY <<- paste(this_data$ezDUMMY,this_data[,names(this_data)==this_temp_not_numeric],sep='')
									}
									names(this_data)[ncol(this_data)] <<- dummy
								}
								this_data[,names(this_data)==dummy] <<- ordered(this_data[,names(this_data)==dummy])
								temp[i] = paste(
									ifelse(numeric_fixed_num>1,ifelse(length(gam_smooth)>1,gam_smooth[2],gam_smooth),gam_smooth[1])
									,'('
									, paste(temp_numeric,collapse=',')
									, ',k=c('
									, paste(k,collapse=',')
									, '),by='
									, dummy
									, ',bs="'
									, gam_bs
									, '")'
									, sep=''
								)
							}
						}
					}
					return(paste(temp,collapse='+'))
				}
				restricted = convert_to_gam_formula(paste('y~',gsub(':','*',effect),'-',effect))
				unrestricted = convert_to_gam_formula(paste('y~',gsub(':','*',effect)))
			}					
		}else{
			if(this_height==1){
				restricted = 'NULL'
				unrestricted = effect
			}else{
				restricted = paste(gsub(':','*',effect),'-',effect)
				unrestricted = paste(gsub(':','*',effect))
			}
		}
		make_formula_pretty = function(formula){
			formula_terms = terms(eval(parse(text=formula)))
			formula_terms = attr(attr(formula_terms,'factors'),'dimnames')[[2]]
			for(i in grep('|',formula_terms,fixed=TRUE)){
				formula_terms[i] = paste('(',formula_terms[i],')')
			}
			formula_terms = gsub(' ','',formula_terms)
			formula_text = paste(formula_terms,collapse=' + ')
			formula_text = paste(as.character(dv),formula_text,sep=' ~ ')
			return(formula_text)
		}
		restricted_formula = make_formula_pretty(
			paste(
				formula_base
				, '+'
				, restricted
			)
		)
		unrestricted_formula = make_formula_pretty(
			paste(
				formula_base
				, '+'
				, unrestricted
			)
		)
		do_fit = function(formula){
			original_warn = options(warn=-1)
			w = NULL
			e = NULL
			fit = NULL
			if((!is.null(numeric_covariates)&cov_gam)|(fix_gam&(numeric_fixed_num>0))){
				try(
					fit <- withCallingHandlers(
						{ 
							eval(parse(text=paste(
								ifelse(
									use_bam
									, 'bam'
									, 'gam'
								)
								, "( formula ="
								, formula
								, ", data = this_data , method = 'ML' , family = family"
								, ifelse(
									!is.null(gam_args)
									, paste(',',gam_args)
									, ''
								)
								, ")"
								, sep = ''
							)))
						}
						, warning = function(x) {w<<-c(w,x$message)}
						, error = function(x) {e<<-c(e,x$message)}
					)
					, silent = T
				)
			}else{
				try(
					fit <- withCallingHandlers(
						{ 
							if(identical(family,gaussian)|identical(family,'gaussian')){
								eval(parse(text=paste(
									"lmer( formula = "
									, formula
									, ", data = this_data , REML = FALSE"
									, ifelse(
										!is.null(mer_args)
										, paste(',',mer_args)
										, ''
									)
									, ")"
								)))
							}else{
								eval(parse(text=paste(
									"glmer( formula ="
									, formula
									, ", data = this_data, family = family"
									, ifelse(
										!is.null(mer_args)
										, paste(',',mer_args)
										, ''
									)
									, ")"
								)))
							}
						}
						, warning = function(x) {w<<-c(w,x$message)}
						, error = function(x) {e<<-c(e,x$message)}
					)
					, silent = T
				)
			}
			options(original_warn)
			out_from_do_fit = list(
				fit = fit
				, errors = e
				, warnings = w
			)
			return(out_from_do_fit)
		}
		out_from_process_term = list(
			summary = data.frame(
				effect = term_labels[this_term_num]
				, errors = NA
				, warnings = NA
				, bits = NA
			)
			, formulae = list(
				restricted = NA
				, unrestricted = NA
			)
			, errors = list(
				restricted = NA
				, unrestricted = NA
			)
			, warnings = list(
				restricted = NA
				, unrestricted = NA
			)
		)
		if(return_models){
			out_from_process_term$models = list(restricted=NA,unrestricted=NA)
		}
		out_from_process_term$formulae$restricted = restricted_formula
		out_from_process_term$formulae$unrestricted = unrestricted_formula
		if(parallelism=='pair'){
			out = llply(
				.data = c(unrestricted_formula,restricted_formula)
				, .fun = do_fit
				, .parallel = TRUE
			)
			unrestricted_fit = out[[1]][[1]]
			unrestricted_errors = out[[1]][[2]]
			unrestricted_warnings = out[[1]][[3]]
			restricted_fit = out[[2]][[1]]
			restricted_errors = out[[2]][[2]]
			restricted_warnings = out[[2]][[3]]
			rm(out)
			gc()
			out_from_process_term$summary$errors = ifelse(is.null(unrestricted_errors)&is.null(restricted_errors),F,T)
			out_from_process_term$summary$warnings = ifelse(is.null(unrestricted_warnings)&is.null(restricted_warnings),F,T)
			out_from_process_term$errors$restricted = restricted_errors
			out_from_process_term$errors$unrestricted = unrestricted_errors
			out_from_process_term$warnings$restricted = restricted_warnings
			out_from_process_term$warnings$unrestricted = unrestricted_warnings
			if(!is.null(unrestricted_fit)){
				unrestricted_cLL = correction(unrestricted_fit)*log2(exp(1))
				restricted_cLL = correction(restricted_fit)*log2(exp(1))
				out_from_process_term$summary$bits = restricted_cLL - unrestricted_cLL
			}
			if(!is.null(progress_dir)){
				eval(parse(text=paste("dir.create(paste(progress_dir,'/models/",term_text,"',sep=''))",sep="")))
				eval(parse(text=paste("save(unrestricted_fit, file = paste(progress_dir,'/models/",term_text,"/unrestricted_fit.RData',sep=''))",sep="")))
				eval(parse(text=paste("save(restricted_fit, file = paste(progress_dir,'/models/",term_text,"/restricted_fit.RData',sep=''))",sep="")))
			}else{
				out_from_process_term$models$unrestricted=unrestricted_fit
				out_from_process_term$models$restricted=restricted_fit
			}
			rm(unrestricted_fit)
			rm(restricted_fit)
			gc()
		}else{
			out = do_fit(unrestricted_formula)
			unrestricted_fit = out[[1]]
			unrestricted_errors = out[[2]]
			unrestricted_warnings = out[[3]]
			rm(out)
			gc()
			out_from_process_term$summary$errors = ifelse(is.null(unrestricted_errors),F,T)
			out_from_process_term$summary$warnings = ifelse(is.null(unrestricted_warnings),F,T)
			out_from_process_term$errors$unrestricted = unrestricted_errors
			out_from_process_term$warnings$unrestricted = unrestricted_warnings
			if(!is.null(unrestricted_fit)){
				unrestricted_cLL = correction(unrestricted_fit)*log2(exp(1))
				if(!is.null(progress_dir)){
					eval(parse(text=paste("dir.create(paste(progress_dir,'/models/",term_text,"',sep=''))",sep="")))
					eval(parse(text=paste("save(unrestricted_fit, file = paste(progress_dir,'/models/",term_text,"/unrestricted_fit.RData',sep=''))",sep="")))
				}else{
					if(return_models){
						out_from_process_term$models$unrestricted = unrestricted_fit
					}
				}
				rm(unrestricted_fit)
				gc()
				out = do_fit(restricted_formula)
				restricted_fit = out[[1]]
				restricted_errors = out[[2]]
				restricted_warnings = out[[3]]
				rm(out)
				gc()
				out_from_process_term$summary$errors = ifelse(is.null(restricted_errors)&!out_from_process_term$summary$errors,F,T)
				out_from_process_term$summary$warnings = ifelse(is.null(restricted_warnings)&!out_from_process_term$summary$warnings,F,T)
				out_from_process_term$errors$restricted = restricted_errors
				out_from_process_term$warnings$restricted = restricted_warnings
				restricted_cLL = correction(restricted_fit)*log2(exp(1))
				out_from_process_term$summary$bits = restricted_cLL - unrestricted_cLL
				if(!is.null(progress_dir)){
					eval(parse(text=paste("save(restricted_fit, file = paste(progress_dir,'/models/",term_text,"/restricted_fit.RData',sep=''))",sep="")))
				}else{
					if(return_models){
						out_from_process_term$models$restricted = restricted_fit
					}
				}			
				rm(restricted_fit)
				gc()
			}
		}
		if(!is.null(progress_dir)){
			eval(parse(text=paste("save(out_from_process_term, file = paste(progress_dir,'/",term_text,".RData',sep=''))",sep="")))
		}
		bits = format(c(out_from_process_term$summary$bits,-1), digits=1, nsmall = 2,scientific=T)
		cat(
			c(
				bits[1]
				, ifelse(out_from_process_term$summary$error,'X','-')
				, ifelse(out_from_process_term$summary$warning,'X','-')
				, term_labels[this_term_num]
				, '\n\r'
			)
			, sep = ' '
		)
		flush.console()
		return(out_from_process_term)
	}
	if(parallelism!='full'){
		out_from_terms = list()
		for(this_term_num in 1:length(term_labels)){
			out_from_terms[[this_term_num]] = process_term(this_term_num)
		}
	}else{
		out_from_terms = llply(
			.data = 1:length(term_labels)
			, .fun = process_term
			, .parallel = TRUE
		)
	}
	out_from_ezMixed = list()
	out_from_ezMixed$summary = ldply(
		.data = out_from_terms
		, .fun = function(x){
			return(x$summary)
		}
	)
	out_from_ezMixed$formulae = llply(
		.data = out_from_terms
		, .fun = function(x){
			return(x$formulae)
		}
	)
	names(out_from_ezMixed$formulae) = out_from_ezMixed$summary$effect
	out_from_ezMixed$errors = llply(
		.data = out_from_terms
		, .fun = function(x){
			return(x$errors)
		}
	)
	names(out_from_ezMixed$errors) = out_from_ezMixed$summary$effect
	out_from_ezMixed$warnings = llply(
		.data = out_from_terms
		, .fun = function(x){
			return(x$warnings)
		}
	)
	names(out_from_ezMixed$warnings) = out_from_ezMixed$summary$effect
	if(return_models){
		out_from_ezMixed$models = llply(
			.data = out_from_terms
			, .fun = function(x){
				return(x$models)
			}
		)
		names(out_from_ezMixed$models) = out_from_ezMixed$summary$effect
	}
	cat('Time taken for ezMixed() to complete:',round(proc.time()[3]-start),'seconds\n')
	if(alarm){
		alarm()
	}
	return(out_from_ezMixed)
}
