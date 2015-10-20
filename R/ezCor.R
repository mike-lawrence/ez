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
	, test_correction = 'none'
){
	ntests = ((((ncol(data)-1)^2)-(ncol(data)-1))/2)
	if(test_correction[1]=='bonferroni'){
		test_alpha = test_alpha/ntests
	}else{
		if(test_correction[1]=='sidak'){
			test_alpha = 1-(1-test_alpha)^(1/ntests)
		}
	}
	for(i in 1:length(data)){
		data[,i]=(data[,i]-mean(data[,i],na.rm=T))/sd(data[,i],na.rm=T)
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
			toss = is.na(x) | is.na(y)
			x = x[!toss]
			y = y[!toss]
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
			d = density(x$value[!is.na(x$value)],adjust=density_adjust)
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
	#ggplot() +
	#  geom_point(aes_string(x = "x", y = "y"), data = z, alpha = point_alpha)
	points_layer = layer(
		geom = 'point'
		, params = list(
			alpha = point_alpha
			, na.rm = TRUE
		)
		, stat = "identity"
		, position = "identity"
		, data = z
		, mapping = aes_string(
			x = 'x'
			, y = 'y'
		)
	)
	lm_line_layer = layer(
		geom = 'line'
		, stat = 'smooth'
		, position = "identity"
		, params = list(
			colour = lm_colour
			, method = 'lm'
			, na.rm = TRUE
		)
		, data = z
		, mapping = aes_string(
			x = 'x'
			, y = 'y'
		)
	)
	lm_ribbon_layer = layer(
		geom = 'ribbon'
		, stat = 'smooth'
		, position = "identity"
		, params = list(
			fill = ci_colour
			, alpha = ci_alpha
			, method = 'lm'
			, na.rm = TRUE
		)
		, data = z
		, mapping = aes_string(
			x = 'x'
			, y = 'y'
		)
	)
	cor_text_layer = layer(
		geom = 'text'
		, stat = "identity"
		, position = "identity"
		, data = z_cor
		, mapping = aes_string(
			label = 'cor'
			, size = 'rsq'
			, colour = 'p'
		)
		, params = list(
		  x = 0
		  , y = 0
		  , na.rm = TRUE
		)
	)
	dens_layer = layer(
		geom = 'ribbon'
		, stat = "identity"
		, position = "identity"
		, params = list(
			colour = 'transparent'
			, fill = 'white'
			, na.rm = TRUE
		)
		, data = dens
		, mapping = aes_string(
			x = 'x'
			, ymax = 'ymax'
			, ymin = 'ymin'
		)
	)
	label_layer = layer(
		geom = 'text'
		, stat = "identity"
		, position = "identity"
		, params = list(
			colour = label_colour
			, size = label_size
			, alpha = label_alpha
			, na.rm = TRUE
		)
		, data = labels
		, mapping = aes_string(
			x='x'
			, y='y'
			, label='label'
		)
	)
	y_lab = NULL
	x_lab = NULL
	f = facet_grid(y_lab~x_lab)
	o = theme(
			panel.grid.minor = element_blank()
			,panel.grid.major = element_blank()
			,axis.ticks = element_blank()
			,axis.text.y = element_blank()
			,axis.text.x = element_blank()
			,axis.title.y = element_blank()
			,axis.title.x = element_blank()
			,legend.position='none'
			,strip.background = element_blank()
			,strip.text.x = element_blank()
			,strip.text.y = element_blank()
	)
	x_scale = scale_x_continuous(limits = c( -1*max(abs(dens$x)) , max(abs(dens$x)) ) )
	size_scale = scale_size(limits = c(0,1),range=r_size_lims)
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
