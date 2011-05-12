ezPrecis <-
function(
	data
	, transpose = TRUE
){
	cat('Data frame dimensions:',nrow(data),'rows,',ncol(data),'columns\n')
	to_return = as.data.frame(
		matrix(
			NA
			, 5
			, dim(data)[2]
		)
	)
	names(to_return) = names(data)
	row.names(to_return) = c('type','missing','values','min','max')
	for(i in 1:ncol(data)){
		to_return[2,i] = sum(is.na(data[,i]))
		if(all(is.na(data[,i]))){
			to_return[1,i] = NA
			to_return[3,i] = NA
			to_return[4,i] = NA
			to_return[5,i] = NA
		}else{
			to_return[3,i] = length(unique(data[,i]))
			if(is.factor(data[,i])){
				to_return[1,i] = 'factor'
				data[,i] = factor(data[,i])
				#if(length(levels(x[,i]))<=2){
				#	to_return[4,i] = levels(x[,i])[1]
				#	if(length(levels(x[,i]))==2){
				#		to_return[5,i] = levels(x[,i])[2]
				#	}
				#}else{
				#	to_return[4:5,i] = 'NA'
				#}
				to_return[4,i] = levels(data[,i])[1]
				to_return[5,i] = levels(data[,i])[length(levels(data[,i]))]
			}else{
				if(is.numeric(data[,i])){
					to_return[1,i] = 'numeric'
					to_return[4,i] = signif(min(data[,i],na.rm=T),digits=getOption('digits'))
					to_return[5,i] = signif(max(data[,i],na.rm=T),digits=getOption('digits'))
				}else{
					if(is.character(data[,i])){
						to_return[1,i] = 'character'
						to_return[4,i] = sort(data[,i])[1]
						to_return[5,i] = rev(sort(data[,i]))[1]
					}else{
						if(is.logical(data[,i])){
							to_return[1,i] = 'logical'
							to_return[4,i] = as.logical(min(data[,i]))
							to_return[5,i] = as.logical(max(data[,i]))
						}else{
							to_return[1,i] = '???'
							to_return[4,i] = NA
							to_return[5,i] = NA
						}
					}
				}
			}
		}
	}
	if(transpose){
		to_return = as.data.frame(t(to_return))
	}
	return(to_return)
}
