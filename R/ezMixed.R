ezMixed <-
function(
	data
	, dv
	, random
	, fixed
	, do_gam_for_numeric_fixed = TRUE
	, covariates = NULL
	, do_gam_for_numeric_covariates = TRUE
	, family = gaussian
	, gam_bs = 'ts'
	, gam_max_k_per_dim = Inf
	, alarm = TRUE
	, term_labels = NULL
	, highest = 0
	, return_models = TRUE
	, correction = AIC
	, progress_dir = NULL
	, resume = FALSE
){
	if(!is.null(progress_dir)){
		dir.create(progress_dir)
		dir.create(paste(progress_dir,'models',sep='/'))
		files = list.files(
			path = progress_dir
			, pattern = '.RData'
		)
		terms_done = str_replace_all(files,'BY',':')
		terms_done = str_replace(terms_done,'.RData','')
	}
	#original_warn <- #options(warn=1)
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
	if(do_gam_for_numeric_covariates){
		for(i in 1:length(covariates)){
			if(is.numeric(data[,names(data)==as.character(covariates[i])])){
				numeric_covariates = c(numeric_covariates,as.character(covariates[i]))
			}
		}		
	}
	numeric_fixed = NULL
	if(do_gam_for_numeric_fixed){
		for(i in 1:length(fixed)){
			if(is.numeric(data[,names(data)==as.character(fixed[i])])){
				numeric_fixed = c(numeric_fixed,as.character(fixed[i]))
			}else{
				data[,names(data)==as.character(fixed[i])] = ordered(data[,names(data)==as.character(fixed[i])])
			}
		}		
	}
	if(is.null(term_labels)){
		to_terms = paste('y~',paste(fixed,collapse='*'))
		from_terms = terms(eval(parse(text=to_terms)))
		term_labels = attr(from_terms,'term.labels')
	}
	if(highest>0){
		term_labels = term_labels[laply((strsplit(term_labels,':')),length)<=highest]
	}
	for(i in 1:length(term_labels)){
		temp = unlist(strsplit(term_labels[i],':'))
		temp = temp[order(temp)]
		term_labels[i] = paste(temp,collapse=':')
	}
	term_labels = term_labels[order(str_count(term_labels,':'),term_labels)]
	if(!is.null(progress_dir)&resume){
		term_labels = term_labels[!(term_labels %in% terms_done)]
	}
	to_return = list()
	to_return$summary = data.frame(
		effect = factor(term_labels,levels=term_labels)
		, error = FALSE
		, warning = FALSE
		, bits = NA
	)
	to_return$formulae = list()
	for(i in 1:length(term_labels)){
		to_return$formulae[[i]] = list(
			restricted = NA
			, unrestricted = NA
		)
		names(to_return$formulae)[i] = term_labels[i]
	}
	to_return$errors = to_return$formulae
	to_return$warnings = to_return$formulae
	if(return_models){
		to_return$models = to_return$formulae
	}
	cat('  bits e w effect\n------ - - ------\n\r')
	old_restricted_formula = ''
	for(this_term_num in 1:length(term_labels)){
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
		effect = term_labels[this_term_num]
		effect_split = strsplit(effect,':')[[1]]
		this_height = length(effect_split)
		if((!is.null(numeric_covariates)&do_gam_for_numeric_covariates)|(do_gam_for_numeric_fixed&(any(effect_split%in%numeric_fixed)))){
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
			formula_base = ifelse(
				is.null(covariates[covariates %in% numeric_covariates])
				, formula_base
				, paste(
					formula_base
					, '+'
					, paste(
						's('
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
			)
			formula_base = ifelse(
				is.null(covariates[!(covariates %in% numeric_covariates)])
				, formula_base
				, paste(
					formula_base
					, '+'
					, paste(
						, covariates[!(covariates %in% numeric_covariates)]
						, collapse = '+'
					)
					, '+'
					, sep = ''
				)
			)			
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
			formula_base = ifelse(
				is.null(covariates)
				, formula_base
				, paste(
					formula_base
					, '+'
					, paste(
						, covariates
						, collapse = '+'
					)
					, '+'
					, sep = ''
				)
			)
		}
		if((do_gam_for_numeric_fixed&(any(effect_split%in%numeric_fixed)))){
			if(this_height==1){
				restricted = 'NULL'
				if(any(effect_split%in%numeric_fixed)){
					k = min(
						gam_max_k_per_dim
						, length(unique(data[,names(data)==effect]))
					)
					unrestricted = paste(
						's('
						, effect
						, ',k='
						, k
						, ',bs="'
						, gam_bs
						, '")'
					)
				}else{
					unrestricted = effect
				}
			}else{
				convert_to_gam_formula = function(formula){
					temp = terms(eval(parse(text=formula)))
					temp = attr(temp,'term.labels')
					for(i in 1:length(temp)){
						temp_split = strsplit(temp[i],':')[[1]]
						temp_numeric = temp_split[temp_split%in%numeric_fixed]
						k = 0
						if(length(temp_numeric)!=0){
							for(this_temp_numeric in temp_numeric){
								k = k + min(c(gam_max_k_per_dim,length(unique(data[,names(data)==this_temp_numeric]))))
							}
							temp2 = unique(data[,names(data)%in%temp_numeric])
							k = ifelse(
								is.data.frame(temp2)
								, nrow(temp2)
								, length(temp2)
							)
							k = min(k,gam_max_k_per_dim)
							temp_not_numeric = temp_split[!(temp_split%in%numeric_fixed)]
							if(length(temp_not_numeric)==0){
								temp[i] = paste(
									's('
									, paste(temp_numeric,collapse=',')
									, ',k='
									, k
									, ',bs="'
									, gam_bs
									, '")'
								)
							}else{
								dummy = paste(temp_not_numeric,collapse='BY')
								#dummy = paste(temp_not_numeric,'ORDERED',collapse='BY',sep='')
								if(!(dummy%in%names(data))){
									data$ezDUMMY <<- ''
									for(this_temp_not_numeric in temp_not_numeric){
										data$ezDUMMY <<- paste(data$ezDUMMY,data[,names(data)==this_temp_not_numeric],sep='')
									}
									names(data)[ncol(data)] <<- dummy
								}
								data[,names(data)==dummy] <<- ordered(data[,names(data)==dummy])
								temp[i] = paste(
									's('
									, paste(temp_numeric,collapse=',')
									, ',k='
									, k
									, ',by='
									, dummy
									, ',bs="'
									, gam_bs
									, '")'
								)
							}
						}
					}
					to_return = paste(temp,collapse='+')
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
		to_return$formulae[[this_term_num]]$restricted = restricted_formula
		to_return$formulae[[this_term_num]]$unrestricted = unrestricted_formula
		do_fit = function(formula,i){
			options(warn=-1)
			w = NULL
			e = NULL
			fit = NULL
			if((!is.null(numeric_covariates)&do_gam_for_numeric_covariates)|(do_gam_for_numeric_fixed&(any(effect_split%in%numeric_fixed)))){
				try(
					fit <- withCallingHandlers(
						{ 
							gam(
								formula = eval(parse(text=formula))
								, family = family
								, data = data
								, method = 'ML'
							)
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
							lmer(
								formula = eval(parse(text=formula))
								, family = family
								, data = data
								, REML = FALSE
							)
						}
						, warning = function(x) {if(x!='extra arguments REML are disregarded'){w<<-c(w,x$message)}}
						, error = function(x) {e<<-c(e,x$message)}
					)
					, silent = T
				)
			}
			options(warn=0)
			if(!is.null(e)){
				to_return$summary$error[this_term_num] <<- TRUE
				to_return$errors[[this_term_num]][[i]] <<- e
			}else{
				if(!is.null(w)){
					to_return$summary$warning[this_term_num] <<- TRUE
					to_return$warnings[[this_term_num]][[i]] <<- w
				}
			}
			return(fit)
		}
		unrestricted_fit = do_fit(unrestricted_formula,2)
		if(!is.null(unrestricted_fit)){
			restricted_fit = do_fit(restricted_formula,1)			
		}else{
			restricted_fit = NULL
		}
		if(return_models){
			to_return$models[[this_term_num]]$restricted = restricted_fit
			to_return$models[[this_term_num]]$unrestricted = unrestricted_fit
		}
		if((!is.null(restricted_fit)) & (!is.null(unrestricted_fit))){
			to_return$summary$bits[this_term_num] = (correction(restricted_fit)-correction(unrestricted_fit))*log2(exp(1))
		}
		if(!is.null(progress_dir)){
			term_text = str_replace_all(term_labels[this_term_num],':','BY')
			dir.create(paste(progress_dir,'models',sep='/'))
			temp<-list(
				summary = to_return$summary[this_term_num,]
				, formulae = to_return$formulae[[this_term_num]]
				, warnings = to_return$warnings[[this_term_num]]
				, errors = to_return$warnings[[this_term_num]]
			)
			eval(parse(text=paste("save(temp, file = paste(progress_dir,'/",term_text,".RData',sep=''))",sep="")))
			eval(parse(text=paste("dir.create(paste(progress_dir,'/models/",term_text,"',sep=''))",sep="")))
			eval(parse(text=paste("save(restricted_fit, file = paste(progress_dir,'/models/",term_text,"/restricted_fit.RData',sep=''))",sep="")))
			eval(parse(text=paste("save(unrestricted_fit, file = paste(progress_dir,'/models/",term_text,"/unrestricted_fit.RData',sep=''))",sep="")))
			rm(temp)
		}
		rm(restricted_fit,unrestricted_fit)
		gc()
		longest_term_char_length = max(nchar(term_labels))
		this_term_char_length = nchar(effect)
		bits = format(c(to_return$summary$bits[this_term_num],-1), digits=1, nsmall = 2,scientific=T)
		cat(
			c(
				bits[1]
				, ifelse(to_return$summary$error[this_term_num],'X','-')
				, ifelse(to_return$summary$warning[this_term_num],'X','-')
				, term_labels[this_term_num]
				, '\n\r'
			)
			, sep = ' '
		)
		flush.console()
	}
	cat('Time taken for ezMixed() to complete:',round(proc.time()[3]-start),'seconds\n')
	if(alarm){
		alarm()
	}
	#options(original_warn)
	return(to_return)
}
