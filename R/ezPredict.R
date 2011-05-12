ezPredict <-
function(
	fit
	, to_predict = NULL
	, numeric_res = 0
){
	data = attr(fit,'frame')
	vars = as.character(attr(attr(data,'terms'),'variables'))
	dv = as.character(vars[2])
	if(is.null(to_predict)){
		if(length(grep('poly(',vars,fixed=TRUE))>0){
			stop('Cannot auto-create "to_predict" when the fitted model contains poly(). Please provide a data frame to the "to_return" argument.')
		}
		vars = vars[3:length(vars)]
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
				temp[[i]] = unique(this_fixed_data)
			}
		}
		to_return = data.frame(expand.grid(temp))
		names(to_return) = data_vars
	}else{
		to_return = to_predict
		data_vars = names(to_predict)
	}
	requested_terms = terms(eval(parse(text=paste(
		dv
		, '~'
		, paste(
			attr(attr(data,'terms'),'term.labels')
			, collapse = '+'
		)
	))))
	to_return$ezDV = 0
	names(to_return)[ncol(to_return)] = dv
	mm = model.matrix(requested_terms,to_return)
	f = fixef(fit)
	value = mm %*% f
	to_return$value = as.numeric(value[,1])
	vf = vcov(fit)
	tc = Matrix::tcrossprod(vf,mm)
	to_return$var = Matrix::diag(mm %*% tc)
	to_return = to_return[,names(to_return)!=dv]
	to_return = to_return[,names(to_return) %in% c(data_vars,'value','var')]	
	return(to_return)
}