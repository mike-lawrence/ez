ezDesign <-
function(
	data
	, x
	, y
	, row = NULL
	, col = NULL
	, cell_border_size = 10
){
	counts = ddply(
		.data = data
		, .variables = structure(as.list(c(x,y,row,col)),class = 'quoted')
		, .fun = function(z){
			to_return = data.frame(
				Count = nrow(z)
			)
			return(to_return)
		}
	)
	y_lab = names(counts)[names(counts)==y]
	x_lab = names(counts)[names(counts)==x]
	names(counts)[names(counts)==y] = 'y'
	names(counts)[names(counts)==x] = 'x'
	if(!is.numeric(counts$x)){
		counts$x = factor(counts$x)
		x_vals = as.character(levels(counts$x))
	}else{
		x_vals = as.character(sort(unique(counts$x)))
	}
	counts$x = as.numeric(factor(counts$x))
	if(!is.numeric(counts$y)){
		counts$y = factor(counts$y)
		y_vals = as.character(levels(counts$y))
	}else{
		y_vals = as.character(sort(unique(counts$y)))
	}
	counts$y = as.numeric(factor(counts$y))
	if(length(unique(counts$y))>length(unique(counts$x))){
		cell_border_size = cell_border_size/length(unique(counts$y))
	}else{
		cell_border_size = cell_border_size/length(unique(counts$x))		
	}
	if(!is.null(row)){
		if(!is.factor(counts[,names(counts)==row])){
			counts[,names(counts)==row] = factor(counts[,names(counts)==row])
		}
		levels(counts[,names(counts)==row]) = paste(
			names(counts)[names(counts)==row]
			, levels(counts[,names(counts)==row])
			, sep = ' = '
		)
		names(counts)[names(counts)==row] = 'row'
	}
	if(!is.null(col)){
		if(!is.factor(counts[,names(counts)==col])){
			counts[,names(counts)==col] = factor(counts[,names(counts)==col])
		}
		levels(counts[,names(counts)==col]) = paste(
			names(counts)[names(counts)==col]
			, levels(counts[,names(counts)==col])
			, sep = ' = '
		)
		names(counts)[names(counts)==col] = 'col'
	}
	p = ggplot(
		data = counts
		,aes(
			ymin = y-.5
			, ymax = y+.5
			, xmin = x-.5
			, xmax = x+.5
			, fill = Count	
		)
	)+
	geom_rect()+
	labs(x=x_lab,y=y_lab)+
	opts(
		panel.grid.major = theme_blank()
		, panel.grid.minor = theme_blank()
		, legend.background = theme_rect(colour='transparent',fill='transparent')
	)
	if(max(counts$Count)==min(counts$Count)){
		p = p + scale_fill_gradient(
			high = muted('blue')
			, low = muted('red')
			, limit = c(0,max(counts$Count))
			, breaks = max(counts$Count)
		)
	}else{
		p = p + scale_fill_gradient(
			high = muted('blue')
			, low = muted('red')
			, limit = c(min(counts$Count),max(counts$Count))
			, breaks = c(min(counts$Count),max(counts$Count))
		)
	}
	if(cell_border_size>0){
		p = p + geom_rect(
			size = cell_border_size
			, colour = 'grey90'
			, legend = FALSE
		)
	}
	p = p + scale_x_continuous(
		breaks = sort(unique(counts$x))
		, labels = x_vals
	)
	p = p + scale_y_continuous(
		breaks = sort(unique(counts$y))
		, labels = y_vals
	)
	if(!is.null(row)){
		if(!is.null(col)){
			p = p + facet_grid(row~col)
		}
		else{
			p = p + facet_grid(row~.)
		}
	}else{
		if(!is.null(col)){
			p = p + facet_grid(.~col)
		}
	}
	return(p)
}