ezPlot <-
function (
	data
	, dv
	, wid
	, within = NULL
	, within_full = NULL
	, between = NULL
	, between_full = NULL
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
	, row_y_free = FALSE
){
	args_to_check = c('dv','wid','within','between','within_full','between_full','diff','x','split','row','col','to_numeric')
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
		data = ezStats(data=data,dv=dv,wid=wid,within=within,between=between,between_full=between_full,diff=diff,reverse_diff=reverse_diff,type=type,check_args=F)
	}else{
		if(!is.null(row)){
			stop('You may not specify a variable to "row" when also specifying multiple dvs.')
		}
		if(is.data.frame(data)|(length(dv)!=length(data))){		
			stop('When specifying multiple dvs, you must provide a list to "data" with as many elements as there are dvs..')
		}
		row = .(dv)
		data_combined = NULL
		for(this_dv in 1:length(dv)){
			this_dot_dv = structure(as.list(c(dv[this_dv])),class = 'quoted')
			if(!is.data.frame(data)){
				data_combined = rbind(
					data_combined
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
							, check_args = FALSE
						)
						, dv = as.character(dv[this_dv])
					)
				)
			}else{
				data_combined = rbind(
					data_combined
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
		data = data_combined
		if(!is.null(dv_levs)){
			if(!is.null(dv_labs)){
				data$dv = factor(data$dv, levels = dv_levs, labels = dv_labs)
			}else{
				data$dv = factor(data$dv, levels = dv_levs)				
			}
		}else{
			 if(!is.null(dv_labs)){
				levels(data$dv) = dv_labs
			}
		}
	}
	if(is.null(bar_size)){
		bar_size = data$FLSD
	}
	data$ymin = data$Mean-bar_size/2
	data$ymax = data$Mean+bar_size/2
	for(i in to_numeric){
		data[,names(data) == i] = as.numeric(as.character(data[,names(data) == i]))
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
		if(!is.numeric(data[,names(data)==x])){
			bar_width = .25
		}
	}
	names(data)[names(data) == x] = 'x'
	if(!is.null(split)){
		names(data)[names(data) == split] = 'split'
	}
	if(!is.null(row)){
		names(data)[names(data) == row] = 'row'
	}
	if(!is.null(col)){
		names(data)[names(data) == col] = 'col'
	}
	p = ggplot(
		data = data
		,aes(
			y = Mean
			,x = x
		)
	)
	if(!is.null(split)){
		p = p+geom_point(
			aes(
				colour = split
				, shape = split
			)
			, alpha = .8
		)
		#p = p+opts(legend.background = theme_rect(colour='transparent',fill='transparent')) #used to be necessary
		if(!is.null(split_lab)){
			p = p+labs(colour = split_lab,shape = split_lab)
		}
		if(do_lines){
			p = p+geom_line(
				aes(
					colour = split
					,linetype = split
					,x = as.numeric(x)
				)
				, alpha = .8
			)
			if(!is.null(split_lab)){
				p = p+labs(linetype = split_lab)
			}
		}
		if(do_bars){
			p = p+geom_errorbar(
				aes(
					colour = split
					,ymin = ymin
					,ymax = ymax
				)
				,linetype = 1
				,guide = 'none'
				,width = bar_width
				, alpha = .5
			)
		}
	}else{
		p = p+geom_point()
		if(do_lines){
			p = p+geom_line(aes(x = as.numeric(x)))
		}
		if(do_bars){
			p = p+geom_errorbar(
				aes(
					ymin = ymin
					,ymax = ymax
				)
				,linetype = 1
				,guide = 'none'
				,width = bar_width
				,alpha = .5
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
	return(p)
}

