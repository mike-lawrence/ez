ezBootPlot <-
function(
	from_ezBoot
	, confidence = .95
	, x
	, split = NULL
	, row = NULL
	, col = NULL
	, do_lines = TRUE
	, bar_width = NULL
	, to_numeric = NULL
	, x_lab = NULL
	, y_lab = NULL
	, split_lab = NULL
	, levels = NULL
	, diff = NULL
	, reverse_diff = NULL
	, row_y_free = FALSE
	, alarm = TRUE
	, do_plot = TRUE
){
	if(!is.logical(do_lines)){
		stop('"do_lines" must be either TRUE or FALSE.')
	}
	if(!is.null(levels)){
		for(i in 1:length(levels)){
			this_iv = names(levels)[i]
			from_ezBoot$cells[,names(from_ezBoot$cells)==this_iv] = factor(from_ezBoot$cells[,names(from_ezBoot$cells)==this_iv])
			if('new_order' %in% names(levels[[i]])){
				from_ezBoot$cells[,names(from_ezBoot$cells)==this_iv] = factor(from_ezBoot$cells[,names(from_ezBoot$cells)==this_iv],levels=levels[[i]]$new_order)
			}
			if('new_names' %in% names(levels[[i]])){
				levels(from_ezBoot$cells[,names(from_ezBoot$cells)==this_iv]) = levels[[i]]$new_names
			}
			from_ezBoot$boots[,names(from_ezBoot$boots)==this_iv] = factor(from_ezBoot$boots[,names(from_ezBoot$boots)==this_iv])
			if('new_order' %in% names(levels[[i]])){
				from_ezBoot$boots[,names(from_ezBoot$boots)==this_iv] = factor(from_ezBoot$boots[,names(from_ezBoot$boots)==this_iv],levels=levels[[i]]$new_order)
			}
			if('new_names' %in% names(levels[[i]])){
				levels(from_ezBoot$boots[,names(from_ezBoot$boots)==this_iv]) = levels[[i]]$new_names
			}
		}
	}
	if(any(confidence>=1) | any(confidence<=0)){
		stop('"confidence" must be either greater than 0 and less than 1.')
	}
	for(i in to_numeric){
		from_ezBoot$cells[,names(from_ezBoot$cells) == i] = as.numeric(as.character(from_ezBoot$cells[,names(from_ezBoot$cells) == i]))
		from_ezBoot$boots[,names(from_ezBoot$boots) == i] = as.numeric(as.character(from_ezBoot$boots[,names(from_ezBoot$boots) == i]))
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
			if(!is.numeric(from_ezBoot$cells[,names(from_ezBoot$cells)==x])){
				bar_width = .25
			}
		}
	}
	#cat('ezBootPlot: Collapsing cells to requested design...')
	cells = ddply(
		.data = idata.frame(from_ezBoot$cells)
		, .variables = structure(as.list(c(x,split,row,col,diff)),class = 'quoted')
		, .fun = function(x){
			to_return = data.frame(
				value = mean(x$value)
			)
			return(to_return)
		}
	)
	#cat('\nezBootPlot: Collapsing boots to requested design...')
	boots = from_ezBoot$boots
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
			#cat('\nezBootPlot: Computing requested this_difference score within cells...')
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
	#cat('\nezBootPlot: Computing confidence intervals...')
	boot_stats = list()
	for(i in 1:length(confidence)){
		if(is.null(x)){
			boot_stats[[i]] = data.frame(
				lo = quantile(boots$value,(1-confidence[i])/2)
				, hi = quantile(boots$value,1-(1-confidence[i])/2)
			)
		}else{
			boot_stats[[i]] = ddply(
				.data = idata.frame(boots)
				, .variables = structure(as.list(c(x,split,row,col)),class = 'quoted')
				, .fun = function(x){
					to_return = data.frame(
						lo = quantile(x$value,(1-confidence[i])/2)
						, hi = quantile(x$value,1-(1-confidence[i])/2)
					)
					return(to_return)
				}
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
	to_return = list()
	to_return$cells = cells
	to_return$boots = boots
	to_return$boot_stats = boot_stats
	#cat('\nezBootPlot: Building plot...')
	if(do_plot){
		p = ggplot(
			data = cells
			, mapping = aes(
				x = x
			)
		)
		if(!is.null(split)){
			p = p+geom_point(
				aes(
					colour = split
					, shape = split
					, y = value
				)
				, alpha = .8
			)
			if(!is.null(split_lab)){
				p = p+labs(colour = split_lab,shape = split_lab)
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
			for(i in 1:length(confidence)){
				p = p+geom_errorbar(
					data = boot_stats[[i]]
					, mapping = aes(
						colour = split
						, ymin = lo
						, ymax = hi
					)
					, linetype = 1
					, legend = FALSE
					, width = bar_width[i]
					, alpha = .5
				)
			}
		}else{
			p = p+geom_point(
				mapping = aes(
					y = value
				)
			)
			if(do_lines){
				p = p+geom_line(
					mapping = aes(
						x = as.numeric(x)
						, y = value
					)
				)
			}
			for(i in 1:length(confidence)){
				p = p+geom_errorbar(
					data = boot_stats[[i]]
					, mapping = aes(
						, ymin = lo
						, ymax = hi
					)
					, linetype = 1
					, legend = FALSE
					, width = bar_width[i]
					, alpha = .5
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
		to_return$plot = p
	}
	#cat('\nezBootPlot: Done.\n')
	if(alarm){
		alarm()
	}
	return(to_return)
}

