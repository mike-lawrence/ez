ezPlot2 <-
function(
	predictions
	, confidence = .95
	, x = NULL
	, split = NULL
	, row = NULL
	, col = NULL
	, do_lines = TRUE
	, ribbon = FALSE
	, bar_width = NULL
	, to_numeric = NULL
	, x_lab = NULL
	, y_lab = NULL
	, split_lab = NULL
	, levels = NULL
	, diff = NULL
	, reverse_diff = NULL
	, row_y_free = FALSE
	, alarm = FALSE
	, do_plot = TRUE
	, parallel = FALSE
){
	warning('This dev version of ezPlot2 has changed such that it now only returns a ggplot2 object by default (meaning you don\'t have to "print(myPlot$plot)" but can instead "print(myPlot)"). If you want the cell and bootstrap statistics, set the "do_plot" argument to FALSE and ezPlot2 will return a data frame. This message will be removed soon (when ez 4.0 is released?).')
	args_to_check = c('x','split','row','col','diff','to_numeric')
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
	if(!is.logical(do_lines)){
		stop('"do_lines" must be either TRUE or FALSE.')
	}
	#code a check here that the difference variables each only have 2 levels
	cells = predictions$cells
	boots = predictions$boots
	for(i in as.character(c(x,split,row,col,diff))){
		if(is.factor(cells[,names(cells)==i])){
			if(length(unique(cells[,names(cells)==i]))!=length(levels(cells[,names(cells)==i]))){
				warning(paste('You have removed one or more levels from variable "',i,'". Refactoring for ezPlot2.',sep=''),immediate.=TRUE,call.=FALSE)
				cells[,names(cells)==i] = factor(cells[,names(cells)==i])
				boots[,names(boots)==i] = factor(boots[,names(boots)==i])
			}
		}
	}
	if(!is.null(levels)){
		for(i in 1:length(levels)){
			this_iv = names(levels)[i]
			cells[,names(cells)==this_iv] = factor(cells[,names(cells)==this_iv])
			if('new_order' %in% names(levels[[i]])){
				cells[,names(cells)==this_iv] = factor(cells[,names(cells)==this_iv],levels=levels[[i]]$new_order)
			}
			if('new_names' %in% names(levels[[i]])){
				levels(cells[,names(cells)==this_iv]) = levels[[i]]$new_names
			}
			boots[,names(boots)==this_iv] = factor(boots[,names(boots)==this_iv])
			if('new_order' %in% names(levels[[i]])){
				boots[,names(boots)==this_iv] = factor(boots[,names(boots)==this_iv],levels=levels[[i]]$new_order)
			}
			if('new_names' %in% names(levels[[i]])){
				levels(boots[,names(boots)==this_iv]) = levels[[i]]$new_names
			}
		}
	}
	if(any(confidence>=1) | any(confidence<=0)){
		stop('"confidence" must be either greater than 0 and less than 1.')
	}
	for(i in to_numeric){
		cells[,names(cells) == i] = as.numeric(as.character(cells[,names(cells) == i]))
		boots[,names(boots) == i] = as.numeric(as.character(boots[,names(boots) == i]))
	}
	if(!is.null(bar_width)){
		if(!is.numeric(bar_width)){
			stop('"bar_width" must be numeric.')
		}else{
			if(any(bar_width<=0)){
				stop('"bar_width" must be > 0.')
			}
		}
		if((length(bar_width)!=1)){
			if(length(confidence)==1){
				stop('Too many values supplied to "bar_width".')
			}else{
				if(length(bar_width)!=length(confidence)){
					stop('"bar_width" must have a length of either 1 or the same length as "confidence".')
				}
			}
		}
	}else{
		if(do_plot){
			if(!is.numeric(cells[,names(cells)==x])){
				bar_width = .25
			}
		}
	}
	cells = ddply(
		.data = idata.frame(cells)
		, .variables = structure(as.list(c(x,split,row,col,diff)),class = 'quoted')
		, .fun = function(z){
			to_return = data.frame(
				value = mean(z$value)
			)
			return(to_return)
		}
		, .parallel = parallel
	)
	boots = boots
	num_it = length(unique(boots$iteration))
	if((num_it*nrow(cells))!=nrow(boots)){
		boots = boots[order(boots$iteration),]
		if(!is.null(x)){
			boots = boots[order(boots[,names(boots)==x]),]
		}
		if(!is.null(split)){
			boots = boots[order(boots[,names(boots)==split]),]
		}
		if(!is.null(row)){
			boots = boots[order(boots[,names(boots)==row]),]
		}
		if(!is.null(col)){
			boots = boots[order(boots[,names(boots)==col]),]
		}
		if(!is.null(diff)){
			boots = boots[order(boots[,names(boots)==diff]),]
		}
		temp = matrix(boots$value,nrow=num_it*nrow(cells),byrow=T)
		boots = boots[((1:nrow(boots))%%ncol(temp))==1,names(boots) %in% as.character(c(x,split,row,col,diff,'iteration'))]
		boots$value = rowMeans(temp)
	}
	if(!is.null(diff)){
		if(is.null(reverse_diff)){
			reverse_diff = rep(F,times=length(diff))
		}else{
			if(length(reverse_diff)!=length(diff)){
				stop('"reverse_diff" must be either NULL or the same length as "diff".')
			}
		}
		for(i in 1:length(diff)){
			this_diff = diff[i]
			if(reverse_diff[i]){
				cells[,names(cells)==as.character(this_diff)] = factor(
					cells[,names(cells)==as.character(this_diff)]
					, levels = rev(levels(cells[,names(cells)==as.character(this_diff)]))
				)
				boots[,names(boots)==as.character(this_diff)] = factor(
					boots[,names(boots)==as.character(this_diff)]
					, levels = rev(levels(boots[,names(boots)==as.character(this_diff)]))
				)
			}
			warning(paste('Collapsing "',as.character(this_diff),'" to a difference score ("',levels(cells[,names(cells)==as.character(this_diff)])[1],'"-"',levels(cells[,names(cells)==as.character(this_diff)])[2],'").',sep=''),immediate.=TRUE,call.=FALSE)
			temp = cells[cells[,names(cells)==as.character(this_diff)]==(levels(cells[,names(cells)==as.character(this_diff)])[1]),]
			temp$value = temp$value - cells$value[cells[,names(cells)==as.character(this_diff)]==(levels(cells[,names(cells)==as.character(this_diff)])[2])]
			cells = temp
			rm(temp)
			cells = cells[,names(cells)!=as.character(this_diff)]
			temp = boots[boots[,names(boots)==as.character(this_diff)]==(levels(boots[,names(boots)==as.character(this_diff)])[1]),]
			temp$value = temp$value - boots$value[boots[,names(boots)==as.character(this_diff)]==(levels(boots[,names(boots)==as.character(this_diff)])[2])]
			boots = temp
			rm(temp)
			boots = boots[,names(boots)!=as.character(this_diff)]
		}
	}
	if(do_plot){
		names(cells)[names(cells)==as.character(x)] = 'x'
		if(!is.null(split)){
			names(cells)[names(cells)==as.character(split)] = 'split'
		}
		if(!is.null(row)){
			names(cells)[names(cells)==as.character(row)] = 'row'
		}
		if(!is.null(col)){
			names(cells)[names(cells)==as.character(col)] = 'col'
		}
	}
	boot_stats = list()
	for(i in 1:length(confidence)){
		if(is.null(x)&is.null(row)&is.null(col)){
			boot_stats[[i]] = data.frame(
				lo = quantile(boots$value,(1-confidence[i])/2)
				, hi = quantile(boots$value,1-(1-confidence[i])/2)
			)
		}else{
			boot_stats[[i]] = ddply(
				.data = idata.frame(boots)
				, .variables = structure(as.list(c(x,split,row,col)),class = 'quoted')
				, .fun = function(z){
					to_return = data.frame(
						lo = quantile(z$value,(1-confidence[i])/2)
						, hi = quantile(z$value,1-(1-confidence[i])/2)
					)
					return(to_return)
				}
				, .parallel = parallel
			)
		}
		if(do_plot){
			names(boot_stats[[i]])[names(boot_stats[[i]])==as.character(x)] = 'x'
			if(!is.null(split)){
				names(boot_stats[[i]])[names(boot_stats[[i]])==as.character(split)] = 'split'
			}
			if(!is.null(row)){
				names(boot_stats[[i]])[names(boot_stats[[i]])==as.character(row)] = 'row'
			}
			if(!is.null(col)){
				names(boot_stats[[i]])[names(boot_stats[[i]])==as.character(col)] = 'col'
			}
		}
	}
	if(do_plot){
		p = ggplot(
			data = cells
			, mapping = aes(
				x = x
			)
		)
		if(!is.null(split)){
			for(i in 1:length(confidence)){
				if(!ribbon){
					p = p+geom_errorbar(
						data = boot_stats[[i]]
						, mapping = aes(
							colour = split
							, ymin = lo
							, ymax = hi
						)
						, linetype = 1
						, guide = 'none'
						, width = bar_width[i]
						, alpha = .5
					)
				}else{
					p = p+geom_ribbon(
						data = boot_stats[[i]]
						, mapping = aes(
							fill = split
							, ymin = lo
							, ymax = hi
						)
						, colour = 'transparent'
						, guide = 'none'
						, alpha = .5
					)
				}
			}
			if(!ribbon){
				p = p+geom_point(
					aes(
						colour = split
						, shape = split
						, y = value
					)
					, alpha = .8
				)
			}
			if(!is.null(split_lab)){
				p = p+labs(colour = split_lab,shape = split_lab,linetype = split_lab,fill = split_lab)
			}
			if(do_lines){
				p = p+geom_line(
					aes(
						colour = split
						, linetype = split
						, x = as.numeric(x)
						, y = value
					)
					, alpha = .8
				)
				if(!is.null(split_lab)){
					p = p+labs(linetype = split_lab)
				}
			}
		}else{
			for(i in 1:length(confidence)){
				if(!ribbon){
					p = p+geom_errorbar(
						data = boot_stats[[i]]
						, mapping = aes(
							, ymin = lo
							, ymax = hi
						)
						, linetype = 1
						, guide = 'none'
						, width = bar_width[i]
						, alpha = .5
					)
				}else{
					p = p+geom_ribbon(
						data = boot_stats[[i]]
						, mapping = aes(
							, ymin = lo
							, ymax = hi
						)
						, colour = 'transparent'
						, guide = 'none'
						, alpha = .5
					)
				}
			}
			if(!ribbon){
				p = p+geom_point(
					mapping = aes(
						y = value
					)
				)
			}
			if(do_lines){
				p = p+geom_line(
					mapping = aes(
						x = as.numeric(x)
						, y = value
					)
				)
			}
		}
		if(!is.null(row)){
			if(!is.null(col)){
				if(row_y_free){
					p = p+facet_grid(row~col,scales='free_y')
				}else{
					p = p+facet_grid(row~col)
				}
			}else{
				if(row_y_free){
					p = p+facet_grid(row~.,scales='free_y')
				}else{
					p = p+facet_grid(row~.)
				}
			}
		}else{
			if(!is.null(col)){
				p = p+facet_grid(.~col)
			}
		}
		if(!is.null(x_lab)){
			p = p+labs(x = x_lab)
		}
		if(!is.null(y_lab)){
			p = p+labs(y = y_lab)
		}
		names(cells)[names(cells)=='x'] = as.character(x)
		if(!is.null(split)){
			names(cells)[names(cells)=='split'] = as.character(split)
		}
		if(!is.null(row)){
			names(cells)[names(cells)=='row'] = as.character(row)
		}
		if(!is.null(col)){
			names(cells)[names(cells)=='col'] = as.character(col)
		}
		to_return = p
	}else{
		to_return = data.frame(cells)
		names(to_return)[ncol(to_return)] = 'value'
		to_return$lo = boot_stats[[1]]$lo
		to_return$hi = boot_stats[[1]]$hi
	}
	if(alarm){
		alarm()
	}
	return(to_return)
}

