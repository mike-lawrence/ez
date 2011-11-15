ezPredict <-
function(
	fit
	, to_predict = NULL
	, numeric_res = 0
	, do_parametric_bootstrap = TRUE
	, parametric_bootstrap_iterations = 1e3
	, effect_variability_only = TRUE
){
	fit_class = class(fit)[1]
	if(fit_class=='mer'){
		data = attr(fit,'frame')
		vars = as.character(attr(attr(data,'terms'),'variables'))
		dv = as.character(vars[2])
		vars = vars[3:length(vars)]
	}else{
		if(fit_class=='gam'){
			data = fit$model
			randoms = NULL
			for(i in fit$smooth){
				if(class(i)[1]=='random.effect'){
					randoms = c(randoms,i$term)
				}
			}
			vars = as.character(attr(attr(data,'terms'),'variables'))
			dv = as.character(vars[2])
			vars = vars[3:length(vars)]
			vars = vars[!(vars%in%randoms)]
			BY = vars[str_detect(vars,'BY')]
			vars = vars[!str_detect(vars,'BY')]
		}else{
			stop(paste('ezPredict does not know how to handle fits of class "',fit_class,'"',sep=''))
		}
	}
	if(is.null(to_predict)){
		if(length(grep('poly(',vars,fixed=TRUE))>0){
			stop('Cannot auto-create "to_predict" when the fitted model contains poly(). Please provide a data frame to the "to_return" argument.')
		}
		data_vars = vars[grep('I(',vars,fixed=T,invert=T)]
		temp = list()
		for(i in 1:length(data_vars)){
			this_fixed_data = data[,names(data)==data_vars[i]]
			if(is.numeric(this_fixed_data)&(numeric_res>0)){
				temp[[i]] = seq(
					min(this_fixed_data)
					, max(this_fixed_data)
					, length.out=numeric_res
				)
			}else{
				temp[[i]] = sort(unique(this_fixed_data))
				if(!is.numeric(this_fixed_data)){
					contrasts(temp[[i]]) = contrasts(this_fixed_data)
				}
			}
		}
		to_return = data.frame(expand.grid(temp))
		names(to_return) = data_vars
	}else{
		to_return = to_predict
	}
	data_vars = names(to_return)
	if(fit_class=='gam'){
		for(i in randoms){
			to_return$EZTEMP = data[1,names(data)==i]
			names(to_return)[ncol(to_return)] = i
		}
		for(i in BY){
			to_return$EZTEMP = ''
			for(j in str_split(i,'BY')[[1]]){
				to_return$EZTEMP = paste(to_return$EZTEMP,as.character(to_return[,names(to_return)==j]),sep='')
			}
			to_return$EZTEMP = ordered(to_return$EZTEMP)
			names(to_return)[ncol(to_return)] = i
		}
	}
	to_return$ezDV = 0
	names(to_return)[ncol(to_return)] = dv
	if(fit_class=='mer'){
		requested_terms = terms(eval(parse(text=paste(
			dv
			, '~'
			, paste(
				attr(attr(data,'terms'),'term.labels')
				, collapse = '+'
			)
		))))
		mm = model.matrix(requested_terms,to_return)
		f = fixef(fit)
		v = vcov(fit)
		if(effect_variability_only){
			v[1,] = 0
			v[,1] = 0
		}
	}else{
		mm <- predict(fit,to_return,type="lpmatrix") # get a coefficient matrix
		for(i in randoms){
			mm[,grep(paste('s(',i,')',sep=''),dimnames(mm)[[2]],fixed=T)] = 0 #zero the subject entry	
		}
		f = coef(fit)
		for(i in randoms){
			f[grep(paste('s(',i,')',sep=''),names(f),fixed=T)] = 0 #zero the subject entry	
		}
		v = vcov(fit)
		if(effect_variability_only){
			v[1,] = 0
			v[,1] = 0
		}
		for(i in randoms){
			row = grep(paste('s(',i,')',sep=''),dimnames(v)[[1]],fixed=T)
			col = grep(paste('s(',i,')',sep=''),dimnames(v)[[2]],fixed=T)
			v[row,] = 0
			v[,col] = 0
		}
	}
	value = mm %*% f
	to_return$value = as.numeric(value[,1])
	tc = Matrix::tcrossprod(v,mm)
	to_return$var = Matrix::diag(mm %*% tc)
	to_return = to_return[,names(to_return) %in% c(data_vars,'value','var')]
	if(do_parametric_bootstrap){
		samples = mvrnorm(parametric_bootstrap_iterations,f,v)
		mat = matrix(NA,nrow=nrow(to_return),ncol=parametric_bootstrap_iterations)
		for(i in 1:parametric_bootstrap_iterations){
			mat[,i] <- mm%*%samples[i,]
		}
		if(effect_variability_only){
			mat = t(t(mat)-colMeans(mat))+mean(mat) #remove variability attributable to iteration grand mean
		}
		boots = as.data.frame(to_return[,names(to_return) %in% data_vars])
		names(boots) = data_vars
		boots = cbind(boots,as.data.frame(mat))
		boots = melt(
			data = boots
			, id.vars = names(boots)[1:(ncol(boots)-parametric_bootstrap_iterations)]
			, variable_name = 'iteration'
		)
		to_return = list(
			cells = to_return
			, boots = boots
		)		
	}
	return(to_return)
}
