ezPlot <-
function (
	data
	, dv
	, wid
	, within = NULL
	, within_full = NULL
	, within_covariates = NULL
	, between = NULL
	, between_full = NULL
	, between_covariates = NULL
	, x
	, do_lines = TRUE
	, do_bars = TRUE
	, bar_width = NULL
	, bar_size = NULL
	, split = NULL
	, row = NULL
	, col = NULL
	, to_numeric = NULL
	, x_lab = NULL
	, y_lab = NULL
	, split_lab = NULL
	, levels = NULL
	, diff = NULL
	, reverse_diff = FALSE
	, type = 2
	, dv_levs = NULL
	, dv_labs = NULL
	, y_free = FALSE
	, print_code = FALSE
){
	args_to_check = c('dv','wid','within','between','within_full','between_full','diff','x','split','row','col','to_numeric','within_covariates','between_covariates')
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
	if(!(x %in% within) & !(x %in% between) ){
		stop('"x" not listed in "within" or "between".')
	}
	if(!is.null(split)){
		if(!(split %in% within) & !(split %in% between) ){
			stop('"split" not listed in "within" or "between".')
		}
	}
	if(!is.null(row)){
		if(!(row %in% within) & !(row %in% between) ){
			stop('"row" not listed in "within" or "between".')
		}
	}
	if(!is.null(col)){
		if(!(col %in% within) & !(col %in% between) ){
			stop('"col" not listed in "within" or "between".')
		}
	}
	if(!is.null(to_numeric)){
		if(!(to_numeric %in% within) & !(to_numeric %in% between) ){
			stop('"to_numeric" not listed in "within" or "between".')
		}
	}
	if(!is.logical(do_lines)){
		stop('"do_lines" must be either TRUE or FALSE.')
	}
	if(!is.logical(do_bars)){
		stop('"do_bars" must be either TRUE or FALSE.')
	}
	if(!is.null(bar_size)){
		if(!is.numeric(bar_size)){
			stop('"bar_size" must be numeric.')
		}else{
			if(bar_size<=0){
				stop('"bar_size" must be > 0.')
			}
		}
	}
	if(is.data.frame(data)){
		if(!is.null(levels)){
			for(i in 1:length(levels)){
				this_iv = names(levels)[i]
				data[,names(data)==this_iv] = factor(data[,names(data)==this_iv])
				if('new_order' %in% names(levels[[i]])){
					data[,names(data)==this_iv] = factor(data[,names(data)==this_iv],levels=levels[[i]]$new_order)
				}
				if('new_names' %in% names(levels[[i]])){
					levels(data[,names(data)==this_iv]) = levels[[i]]$new_names
				}
			}
		}
	}else{
		for(j in 1:length(data)){
			if(!is.null(levels)){
				for(i in 1:length(levels)){
					this_iv = names(levels)[i]
					data[[j]][,names(data[[j]])==this_iv] = factor(data[[j]][,names(data[[j]])==this_iv])
					if('new_order' %in% names(levels[[i]])){
						data[[j]][,names(data[[j]])==this_iv] = factor(data[[j]][,names(data[[j]])==this_iv],levels=levels[[i]]$new_order)
					}
					if('new_names' %in% names(levels[[i]])){
						levels(data[[j]][,names(data[[j]])==this_iv]) = levels[[i]]$new_names
					}
				}
			}
		}
	}
	if(length(dv)==1){
		if(!is.data.frame(data)){
			stop('"data" cannot be a list when specifying only one dv.')
		}
		stats = ezStats(data=data,dv=dv,wid=wid,within=within,within_full=within_full,within_covariates=within_covariates,between=between,between_full=between_full,between_covariates=between_covariates,diff=diff,reverse_diff=reverse_diff,type=type,check_args=F)
	}else{
		if(!is.null(row)){
			stop('You may not specify a variable to "row" when also specifying multiple dvs.')
		}
		if(is.data.frame(data)|(length(dv)!=length(data))){		
			stop('When specifying multiple dvs, you must provide a list to "data" with as many elements as there are dvs..')
		}
		row = .(dv)
		stats = NULL
		for(this_dv in 1:length(dv)){
			this_dot_dv = structure(as.list(c(dv[this_dv])),class = 'quoted')
			if(!is.data.frame(data)){
				stats = rbind(
					stats
					, cbind(
						ezStats(
							data = data[[this_dv]]
							, dv = this_dot_dv
							, wid = wid
							, within = within
							, within_full = within_full
							, within_covariates = within_covariates
							, between = between
							, between_full = between_full
							, between_covariates = between_covariates
							, diff = diff
							, reverse_diff = reverse_diff
							, type = type
							, check_args = FALSE
						)
						, dv = as.character(dv[this_dv])
					)
				)
			}else{
				stats = rbind(
					stats
					, cbind(
						ezStats(
							data = data[[this_dv]]
							, dv = this_dot_dv
							, wid = wid
							, within = within
							, within_full = within_full
							, between = between
							, between_full = between_full
							, diff = diff
							, reverse_diff = reverse_diff
							, type = type
						)
						, dv = as.character(dv[this_dv])
					)
				)
			}
		}
		if(!is.null(dv_levs)){
			if(!is.null(dv_labs)){
				stats$dv = factor(stats$dv, levels = dv_levs, labels = dv_labs)
			}else{
				stats$dv = factor(stats$dv, levels = dv_levs)				
			}
		}else{
			 if(!is.null(dv_labs)){
				levels(stats$dv) = dv_labs
			}
		}
	}
	if(is.null(bar_size)){
		bar_size = stats$FLSD
	}
	stats$lo = stats$Mean-bar_size/2
	stats$hi = stats$Mean+bar_size/2
	for(i in to_numeric){
		stats[,names(stats) == i] = as.numeric(as.character(stats[,names(stats) == i]))
	}
	if(!is.null(bar_width)){
		if(!is.numeric(bar_width)){
			stop('"bar_width" must be numeric.')
		}else{
			if(bar_width<=0){
				stop('"bar_width" must be > 0.')
			}
		}
	}else{
		if(!is.numeric(stats[,names(stats)==x])){
			bar_width = .25
		}
	}
	p = paste("ggplot(\n\tdata = stats\n\t, mapping = aes(\n\t\ty = Mean\n\t\t, x = ",x,"\n\t)\n)",sep='')
	if(!is.null(split)){
		p = paste(p,"+\ngeom_point(\n\tmapping = aes(\n\t\tcolour = ",split,"\n\t\t, shape = ",split,"\n\t)\n\t, alpha = .8\n)",sep='')
		if(do_lines){
			p = paste(p,"+\ngeom_line(\n\tmapping = aes(\n\t\tcolour = ",split,"\n\t\t, linetype = ",split,"\n\t\t, x = I(as.numeric(",x,"))\n\t)\n\t, alpha = .8\n)",sep='')
		}
		if(do_bars){
			p = paste(p,"+\ngeom_errorbar(\n\tmapping = aes(\n\t\tcolour = ",split,"\n\t\t, ymin = lo\n\t\t, ymax = hi\n\t)\n\t, linetype = 1\n\t, show_guide = FALSE\n\t, width = ",bar_width,"\n\t, alpha = .5\n)",sep='')
		}
	}else{
		p = paste(p,"+\ngeom_point()",sep='')
		if(do_lines){
			p = paste(p,"+\ngeom_line(\n\tmapping = aes(\n\t\tx = I(as.numeric(",x,"))\n\t)\n)",sep='')
		}
		if(do_bars){
			p = paste(p,"+\ngeom_errorbar(\n\tmapping = aes(\n\t\tymin = lo\n\t\t, ymax = hi\n\t)\n\t, linetype = 1\n\t, show_guide = FALSE\n\t, width = ",bar_width,"\n\t, alpha = .5\n)",sep='')
		}
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
	if(print_code){
		cat(p)
		return(stats)
	}else{
		return(eval(parse(text=p)))
	}
}

