ezCor <- 
function(
	data
	, r_size_lims = c(10,30)
	, point_alpha = .5
	, density_height = 1
	, density_adjust = 1
	, density_colour = 'white'
	, label_size = 10
	, label_colour = 'black'
	, label_alpha = .5
	, lm_colour = 'red'
	, ci_colour = 'green'
	, ci_alpha = .5
	, test_alpha = .05
){
	for(i in 1:length(data)){
		data[,i]=(data[,i]-mean(data[,i]))/sd(data[,i])
	}
	z=data.frame()
	z_cor = data.frame()
	i = 1
	j = i
	while(i<=length(data)){
		if(j>length(data)){
			i=i+1
			j=i
		}else{
			x = data[,i]
			y = data[,j]
			temp=as.data.frame(cbind(x,y))
			temp=cbind(temp,names(data)[i],names(data)[j])
			z=rbind(z,temp)
			this_cor = round(cor(x,y),2)
			this_cor.test = cor.test(x,y)
			this_col = ifelse(this_cor.test$p.value<test_alpha,'a','b')
			this_size = (this_cor)^2
			cor_text = ifelse(
				this_cor==0
				, '0'
				, ifelse(
					this_cor==1
					, '1'
					, ifelse(
						this_cor==-1
						, '-1'
						, ifelse(
							this_cor>0
							,substr(format(c(this_cor,.123456789),digits=2)[1],2,4)
							,paste('-',substr(format(c(this_cor,.123456789),digits=2)[1],3,5),sep='')
						)
					)
				)
			)
			b=as.data.frame(cor_text)
			b=cbind(b,this_col,this_size,names(data)[j],names(data)[i])
			z_cor=rbind(z_cor,b)
			j=j+1
		}
	}
	names(z)=c('x','y','x_lab','y_lab')
	z=z[z$x_lab!=z$y_lab,]
	names(z_cor)=c('cor','p','rsq','x_lab','y_lab')
	z_cor=z_cor[z_cor$x_lab!=z_cor$y_lab,]
	diag = melt(data,measure.vars=names(data))
	names(diag)[1] = 'x_lab'
	diag$y_lab = diag$x_lab
	dens = ddply(
		diag
		, .(x_lab,y_lab)
		, function(x){
			d = density(x$value,adjust=density_adjust)
			d = data.frame(x=d$x,y=d$y)
			d$ymax = d$y*(max(abs(c(z$x,z$y)))*2*density_height)/max(d$y) - max(abs(c(z$x,z$y)))*density_height
			d$ymin = - max(abs(c(z$x,z$y)))*density_height
			return(d)
		}
	)
	labels = ddply(
		diag
		, .(x_lab,y_lab)
		, function(x){
			to_return = data.frame(
				x = 0
				, y = 0
				, label = x$x_lab[1]
			)
			return(to_return)
		}
	)
	points_layer = layer(
		geom = 'point'
		, geom_par = list(
			alpha = point_alpha
		)
		, data = z
		, mapping = aes(
			x = x
			, y = y
		)
	)
	lm_line_layer = layer(
		geom = 'line'
		, geom_params = list(
			colour = lm_colour
		)
		, stat = 'smooth'
		, stat_params = list(method = 'lm')
		, data = z
		, mapping = aes(
			x = x
			, y = y
		)
	)
	lm_ribbon_layer = layer(
		geom = 'ribbon'
		, geom_params = list(
			fill = ci_colour
			, alpha = ci_alpha
		)
		, stat = 'smooth'
		, stat_params = list(method = 'lm')
		, data = z
		, mapping = aes(
			x = x
			, y = y
		)
	)
	cor_text_layer = layer(
		geom = 'text'
		, data = z_cor
		, mapping = aes(
			x=0
			, y=0
			, label=cor
			, size = rsq
			, colour = p
		)
	)
	dens_layer = layer(
		geom = 'ribbon'
		, geom_par = list(
			colour = 'transparent'
			, fill = 'white'
		)
		, data = dens
		, mapping = aes(
			x = x
			, ymax = ymax
			, ymin = ymin
		)
	)
	label_layer = layer(
		geom = 'text'
		, geom_par = list(
			colour = 'black'
			, size = label_size
			, alpha = .5
		)
		, data = labels
		, mapping = aes(
			x=x
			, y=y
			, label=label
		)
	)
	f = facet_grid(y_lab~x_lab)
	o = opts(
		panel.grid.minor = theme_blank()
		,panel.grid.major = theme_blank()
		,axis.ticks = theme_blank()
		,axis.text.y = theme_blank()
		,axis.text.x = theme_blank()
		,axis.title.y = theme_blank()
		,axis.title.x = theme_blank()
		,legend.position='none'
		,strip.background = theme_blank()
		,strip.text.x = theme_blank()
		,strip.text.y = theme_blank()
	)
	x_scale = scale_x_continuous(limits = c( -1*max(abs(dens$x)) , max(abs(dens$x)) ) )
	size_scale = scale_size(limits = c(0,1),to=r_size_lims)
	return(
		ggplot(z_cor)+
		points_layer+
		lm_ribbon_layer+
		lm_line_layer+
		dens_layer+
		label_layer+
		cor_text_layer+
		f+
		o+
		x_scale+
		size_scale
	)
}
