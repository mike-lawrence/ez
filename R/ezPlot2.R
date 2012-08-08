ezPlot2 <-
function(
	preds
	, CI = .95
	, x = NULL
	, split = NULL
	, row = NULL
	, col = NULL
	, do_lines = TRUE
	, ribbon = FALSE
	, CI_alpha = .5
	, point_alpha = .8
	, line_alpha = .8
	, bar_width = NULL
	, to_numeric = NULL
	, x_lab = NULL
	, y_lab = NULL
	, split_lab = NULL
	, levels = NULL
	, diff = NULL
	, reverse_diff = NULL
	, y_free = FALSE
	, alarm = FALSE
	, do_plot = TRUE
	, print_code = FALSE
	, parallel = FALSE
){
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
	cells = preds$cells
	boots = preds$boots
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
	if(any(CI>=1) | any(CI<=0)){
		stop('"CI" must be either greater than 0 and less than 1.')
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
			if(length(CI)==1){
				stop('Too many values supplied to "bar_width".')
			}else{
				if(length(bar_width)!=length(CI)){
					stop('"bar_width" must have a length of either 1 or the same length as "CI".')
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
	boot_stats = NULL
	for(i in 1:length(CI)){
		if(is.null(x)&is.null(row)&is.null(col)){
			boot_stats = rbind(
				boot_stats
				, data.frame(
					lo = quantile(boots$value,(1-CI[i])/2)
					, hi = quantile(boots$value,1-(1-CI[i])/2)
					, CI = CI[i]
				)
			)
		}else{
			boot_stats = rbind(
				boot_stats
				, cbind(
					ddply(
						.data = idata.frame(boots)
						, .variables = structure(as.list(c(x,split,row,col)),class = 'quoted')
						, .fun = function(z){
							to_return = data.frame(
								lo = quantile(z$value,(1-CI[i])/2)
								, hi = quantile(z$value,1-(1-CI[i])/2)
							)
							return(to_return)
						}
						, .parallel = parallel
					)
					, CI = CI[i]
				)
			)
		}
	}
	if(do_plot){
		p = paste("ggplot()",sep='')
		if(!ribbon){
			p = paste(p,"+\ngeom_errorbar(\n\tdata = boot_stats\n\t, mapping = aes(\n\t\tx = ",x,"\n\t\t, ymin = lo\n\t\t, ymax = hi",sep = '')
			if(!is.null(split)){
				p = paste(p,"\n\t\t, colour = ",split,sep = '')
			}
			if(length(CI)>1){
				p = paste(p,"\n\t\t, width = CI",split,sep = '')
			}
			p = paste(p,"\n\t)\n\t, linetype = 1\n\t, show_guide = FALSE",sep = '')
			if(length(CI)==1&(!is.null(bar_width))){
				p = paste(p,"\n\t, width = ",bar_width,sep='')
			}
			p = paste(p,"\n\t, alpha = ",CI_alpha,"\n)",sep='')
			p = paste(p,"+\ngeom_point(\n\tdata = cells\n\t, mapping = aes(\n\t\tx = ",x,"\n\t\t, y = value",sep='')
			if(!is.null(split)){
				p = paste(p,"\n\t\t, colour = ",split,"\n\t\t, shape = ",split,sep='')
			}
			p = paste(p,"\n\t)\n\t, alpha = ",point_alpha,"\n)")
		}else{
			p = paste(p,"+\ngeom_ribbon(\n\tdata = boot_stats\n\t, mapping = aes(\n\t\tx = ",x,"\n\t\t, ymin = lo\n\t\t, ymax = hi",sep = '')
			if(!is.null(split)){
				p = paste(p,"\n\t\t, fill = ",split,sep = '')					
			}
			if(length(CI)>1){
				p = paste(p,"\n\t\t, alpha = CI",split,sep = '')					
			}
			p = paste(p,"\n\t)\n\t, color = 'transparent'\n\t, show_guide = FALSE",sep = '')
			if(length(CI)==1){
				p = paste(p,"\n\t, alpha = ",CI_alpha,sep='')
			}
			p = paste(p,"\n)",sep='')
		}
		if(do_lines){
			p = paste(p,"+\ngeom_line(\n\tdata = cells\n\t, mapping = aes(\n\t\tx = I(as.numeric(",x,"))\n\t\t, y = value",sep='')
			if(!is.null(split)){
				p = paste(p,"\n\t\t, colour = ",split,"\n\t\t, linetype = ",split,sep='')
			}
			p = paste(p,"\n\t)\n\t, alpha = ",line_alpha,"\n)",sep='')
		}
		if(!is.null(row)){
			if(!is.null(col)){
				if(y_free){
					p = paste(p,"+\nfacet_grid(\n\tfacets = ",row," ~ ",col,"\n\t, scales = 'free_y'\n)",sep='')
				}else{
					p = paste(p,"+\nfacet_grid(\n\tfacets = ",row," ~ ",col,"\n)",sep='')
				}
			}else{
				if(y_free){
					p = paste(p,"+\nfacet_grid(\n\tfacets = ",row," ~ .\n\t, scales = 'free_y'\n)",sep='')
				}else{
					p = paste(p,"+\nfacet_grid(\n\tfacets = ",row," ~ .\n)",sep='')
				}
			}
		}else{
			if(!is.null(col)){
				p = paste(p,"+\nfacet_grid(\n\tfacets = . ~ ",col,"\n\t, scales = 'free_y'\n)",sep='')
			}
		}
		if(str_detect(p,"alpha = CI")){
			p = paste(p,"+\nscale_alpha_manual(\n\tvalues = c(",sep='')
			for(i in 1:(length(CI)-1)){
				paste(p,bar_width[i],",",sep='')
			}
			paste(p,bar_width[i+1],")\n)",sep='')
		}
		if(str_detect(p,"width = CI")){
			p = paste(p,"+\nscale_width_manual(\n\tvalues = c(",sep='')
			for(i in 1:(length(CI)-1)){
				paste(p,bar_width[i],",",sep='')
			}
			paste(p,bar_width[i+1],")\n)",sep='')
		}
		if(any(c((!is.null(x_lab)),(!is.null(y_lab)),(!is.null(split_lab))))){
			p = paste(p,'+\nlabs(',sep='')
			if(!is.null(x_lab)){
				p = paste(p,"\n\tx = '",x_lab,"'",sep='')
				if(!is.null(y_lab)){
					p = paste(p,"\n\t, y = '",y_lab,"'",sep='')
				}
				if(!is.null(split_lab)){
					p = paste(p,"\n\t, colour = '",split_lab,"'",sep='')
					p = paste(p,"\n\t, shape = '",split_lab,"'",sep='')
					if(do_lines){
						p = paste(p,"\n\t, linetype = '",split_lab,"'",sep='')
					}
				}
			}else{
				if(!is.null(y_lab)){
					p = paste(p,"\n\ty = '",y_lab,"'",sep='')
					if(!is.null(split_lab)){
						p = paste(p,"\n\t, colour = '",split_lab,"'",sep='')
						p = paste(p,"\n\t, shape = '",split_lab,"'",sep='')
						if(do_lines){
							p = paste(p,"\n\t, linetype = '",split_lab,"'",sep='')
						}
					}
				}else{
					if(!is.null(split_lab)){
						p = paste(p,"\n\t, colour = '",split_lab,"'",sep='')
						p = paste(p,"\n\t, shape = '",split_lab,"'",sep='')
						if(do_lines){
							p = paste(p,"\n\t, linetype = '",split_lab,"'",sep='')
						}
					}
				}
			}
			p = paste(p,'\n)',sep='')
		}
		to_return = p
		if(alarm){
			alarm()
		}
		if(print_code){
			cat(p)
			return(list(cells=cells,boot_stats=boot_stats))
		}else{
			return(eval(parse(text=p)))
		}
	}else{
		if(alarm){
			alarm()
		}
		return(list(cells=cells,boot_stats=boot_stats))
	}
}
