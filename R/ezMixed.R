ezMixed <-
function(
	data
	, dv
	, random
	, fixed
	, fixed_poly = NULL
	, fixed_poly_max = NULL
	, covariates = NULL
	, family = gaussian
	, alarm = TRUE
	, results_as_progress = FALSE
	, highest = 0
	, return_models = FALSE
	, highest_first = TRUE
){
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
	randoms = NULL
	for(i in random){
		randoms = paste(randoms,'(1|',i,')+',sep='')
	}
	if(!is.null(covariates)){
		formula_base = paste(
			dv
			, '~'
			, randoms
			, '('
			, paste(
				covariates
				, collapse = '+'
			)
			, ')'
		)
	}else{
		formula_base = paste(
			dv
			, '~'
			, randoms
		)
		formula_base = substr(formula_base,1,nchar(formula_base)-1)
	}
	to_terms = paste('y~',paste(fixed,collapse='*'))
	from_terms = terms(eval(parse(text=to_terms)))
	term_labels = attr(from_terms,'term.labels')
	if(highest>0){
		term_labels = term_labels[laply((strsplit(term_labels,':')),length)<=highest]
	}
	if(!is.null(fixed_poly)){
		if(is.null(fixed_poly_max)){
			fixed_poly_max = rep(NA,length(fixed_poly))
			for(i in 1:length(fixed_poly)){
				fixed_poly_max[i] = length(unique(data[,names(data)==fixed_poly[i]]))-1
			}
		}else{
			if(length(fixed_poly)!=length(fixed_poly_max)){
				stop(paste('"fixed_poly_max" must be the same length as "fixed_poly"'))
			}
		}
		for(i in 1:length(fixed_poly)){
			temp = term_labels
			var = as.character(fixed_poly[i])
			for(j in 2:fixed_poly_max[i]){
				temp2 = term_labels[str_detect(term_labels,var)]
				temp2 = sub(var,paste('I(',var,'^',j,')',sep=''),temp2)
				temp = c(temp,temp2)
			}
			term_labels = temp
		}
	}
	to_return = list()
	to_return$summary = data.frame(
		effect = factor(term_labels,levels=term_labels)
		, error = FALSE
		, warning = FALSE
		, RLnLu = NA
		, RLnLr = NA
		, DFu = NA
		, DFr = NA
		, L10LRa = NA
		, L10LRb = NA
	)
	to_return$formulae = list()
	to_return$errors = list()
	for(i in 1:length(term_labels)){
		to_return$errors[[i]] = list(
			unrestricted = NA
			, restricted = NA
		)
		names(to_return$errors)[i] = term_labels[i]
	}
	to_return$warnings = to_return$errors
	if(return_models){
		to_return$models = list()
	}
	if(!results_as_progress){
		progress = create_progress_bar('timeCI')
		progress$init(length(term_labels))
	}
	old_restricted_formula = ''
	if(highest_first){
		term_order = rev(1:length(term_labels))
	}else{
		term_order = 1:length(term_labels)
	}
	for(this_term_num in term_order){
		effect = term_labels[this_term_num]
		effect_split = strsplit(effect,':')[[1]]
		this_height = length(effect_split)
		effects_with_poly = grep('I(',effect_split,fixed=T)
		if(length(effects_with_poly)==0){ #no polynomials
			effect_baseline = gsub(':','*',effect)
			unrestricted_formula = paste(
				formula_base
				,'+'
				, effect_baseline
			)				
			restricted_formula = paste(
				formula_base
				, '+'
				, effect_baseline
				, '-'
				, effect
			)
		}else{ #the effect involves polynomials
			effects_without_poly = (1:this_height)[!((1:this_height)%in%effects_with_poly)]
			linear_effects = effect_split[effects_without_poly]
			k = length(linear_effects)
			for(i in effect_split[effects_with_poly]){
				temp = sub('I(','',i,fixed=T)
				temp = strsplit(temp,'^',fixed=T)[[1]][1]
				linear_effects = c(linear_effects,temp)
			}
			effects_formula = paste('(',paste(linear_effects,collapse='*'),')')
			for(i in effect_split[effects_with_poly]){
				temp = sub('I(','',i,fixed=T)
				temp = sub(')','',temp,fixed=T)
				temp = strsplit(temp,'^',fixed=T)[[1]]
				degree = as.numeric(temp[2])
				temp = temp[1]
				temp_effects = effects_formula
				for(j in 2:degree){
					effects_formula = paste(
						effects_formula
						, '+'
						, gsub(
							temp
							, paste('I(',temp,'^',j,')')
							, temp_effects
						)
					)
				}					
			}
			restricted_formula = paste(formula_base,'+',effects_formula,'-',effect)
			unrestricted_formula = paste(formula_base,'+',effects_formula)
		}
		#convert the formulas into an easier to read format
		unrestricted_formula_terms = terms(eval(parse(text=unrestricted_formula)))
		unrestricted_formula_terms = attr(attr(unrestricted_formula_terms,'factors'),'dimnames')[[2]]
		for(i in grep('|',unrestricted_formula_terms,fixed=TRUE)){
			unrestricted_formula_terms[i] = paste('(',unrestricted_formula_terms[i],')')
		}
		unrestricted_formula_terms = gsub(' ','',unrestricted_formula_terms)
		unrestricted_formula_text = paste(unrestricted_formula_terms,collapse=' + ')
		unrestricted_formula_text = paste(as.character(dv),unrestricted_formula_text,sep=' ~ ')
		restricted_formula_terms = terms(eval(parse(text=restricted_formula)))
		restricted_formula_terms = attr(attr(restricted_formula_terms,'factors'),'dimnames')[[2]]
		for(i in grep('|',restricted_formula_terms,fixed=TRUE)){
			restricted_formula_terms[i] = paste('(',restricted_formula_terms[i],')')
		}
		restricted_formula_terms = gsub(' ','',restricted_formula_terms)
		restricted_formula_text = paste(restricted_formula_terms,collapse=' + ')
		restricted_formula_text = paste(as.character(dv),restricted_formula_text,sep=' ~ ')
		#begin the fitting
		unrestricted_fit = NULL
		if(restricted_formula!=old_restricted_formula){
			restricted_fit = NULL
		}
		options(warn=-1)
		w = NULL
		e = NULL
		unrestricted_fit = NULL
		try(
			unrestricted_fit <- withCallingHandlers(
				{ 
					lmer(
						formula = eval(parse(text=unrestricted_formula))
						, family = family
						, data = data
						, REML = FALSE
					)
				}
				, warning = function(w) {w<<-w}
				, error = function(e) {e<<-e}
			)
			, silent = T
		)
		options(warn=0)
		if(!is.null(e)){
			to_return$summary$error[this_term_num] = TRUE
			to_return$errors[[this_term_num]]$unrestricted = e$message
		}else{
			if(!is.null(w)){
				to_return$summary$warning[this_term_num] = TRUE
				to_return$warnings[[this_term_num]]$unrestricted = w$message
			}
			if(restricted_formula!=old_restricted_formula){
				old_restricted_formula = restricted_formula
				options(warn=-1)
				w = NULL
				e = NULL
				restricted_fit = NULL
				try(
					restricted_fit <- withCallingHandlers(
						{ 
							lmer(
								formula = eval(parse(text=restricted_formula))
								, family = family
								, data = data
								, REML = FALSE
							)
						}
						, warning = function(w) {w<<-w}
						, error = function(e) {e<<-e}
					)
					, silent = T
				)
				options(warn=0)
				if(!is.null(e)){
					to_return$summary$error[this_term_num] = TRUE
					to_return$errors[[this_term_num]]$restricted = e$message
				}else{
					if(!is.null(w)){
						to_return$summary$warning[this_term_num] = TRUE
						to_return$warnings[[this_term_num]]$restricted = w$message
					}
				}
			}
		}
		to_return$formulae[[this_term_num]] = list()
		to_return$formulae[[this_term_num]]$restricted = restricted_formula_text
		to_return$formulae[[this_term_num]]$unrestricted = unrestricted_formula_text
		names(to_return$formulae)[this_term_num] = term_labels[this_term_num]
		if(return_models){
			to_return$models[[this_term_num]] = list()
			to_return$models[[this_term_num]]$restricted = restricted_fit
			to_return$models[[this_term_num]]$unrestricted = unrestricted_fit
			names(to_return$models)[this_term_num] = term_labels[this_term_num]
		}
		if((!is.null(restricted_fit)) & (!is.null(unrestricted_fit))){
			restricted_logLik = logLik(restricted_fit)
			unrestricted_logLik = logLik(unrestricted_fit)
			to_return$summary$RLnLu[this_term_num] = as.numeric(unrestricted_logLik)
			to_return$summary$RLnLr[this_term_num] = as.numeric(restricted_logLik)
			to_return$summary$DFu[this_term_num] = attr(unrestricted_logLik,'df')
			to_return$summary$DFr[this_term_num] = attr(restricted_logLik,'df')
			to_return$summary$L10LRa[this_term_num] = (AIC(restricted_fit)-AIC(unrestricted_fit))*log(exp(1), base = 10)
			to_return$summary$L10LRb[this_term_num] = (BIC(restricted_fit)-BIC(unrestricted_fit))*log(exp(1), base = 10)
		}
		if(results_as_progress){
			longest_term_char_length = max(nchar(term_labels))
			this_term_char_length = nchar(effect)
			llrs = format(c(to_return$summary$L10LRa[this_term_num], to_return$summary$L10LRb[this_term_num],-1), digits=3, nsmall = 2,scientific=T)
			cat(
				c(
					term_labels[this_term_num]
					, paste(rep(' ',longest_term_char_length-this_term_char_length),collapse='')
					, ' -> L10LRa = '
					, llrs[1]
					, ' , L10LRb = '
					, llrs[2]
					, '\n'
				)
				, sep = ''
			)
		}else{
			progress$step()
		}
	}
	if(!results_as_progress){
		progress$term()
	}else{
		cat('Time taken for ezMixed() to complete:',round(proc.time()[3]-start),'seconds\n')
	}
	if(alarm){
		alarm()
	}
	#options(original_warn)
	return(to_return)
}
