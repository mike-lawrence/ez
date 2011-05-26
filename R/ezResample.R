ezResample <-
function(
	data
	, wid
	, within = NULL
	, between = NULL
	, resample_within = FALSE
	, resample_between = TRUE
){
	if(resample_between){
		if(!is.null(between)){
			ids = dlply(
				.data = data
				, .variables = between
				, .fun = function(x){
					done = FALSE
					while(!done){
						to_return = sample(as.character(unique(x[,names(x)==as.character(wid)])),replace=T)
						if(length(unique(to_return))>1){
							done = TRUE
						}
					}
					return(to_return)
				}
			)
			ids = unlist(ids)
			names(ids) = NULL
		}else{
			done = FALSE
			while(!done){
				ids = sample(as.character(unique(data[,names(data)==as.character(wid)])),replace=T)
				if(length(unique(ids))>1){
					done = TRUE
				}
			}
		}
		id_list = list()
		for(i in 1:length(ids)){
			id_list[[i]] = list(num=i,this_id=ids[i])
		}
		resampled_data = dlply(
			.data = data[data[,names(data)==as.character(wid)] %in% ids,]
			, .variables = wid
			, .fun = function(x){
				to_return = NULL
				for(i in which(ids==x[1,names(x)==as.character(wid)])){
					x$id = id_list[[i]]$num
					to_return = rbind(to_return,x)
				}
				return(to_return)
			}
			#, .progress = 'time'
		)
		resampled_data = Filter(Negate(empty), resampled_data)
	}	
	if(resample_within){
		if(!resample_between){
			resampled_data = dlply(
				.data = data
				, .variables = structure(as.list(c(wid,within)),class = 'quoted')
				, .fun = function(x){
		 			to_return = x[sample(1:nrow(x),nrow(x),replace=T),]
					return(to_return)
				}
				#, .progress = 'time'
			)
		}else{
			resampled_data = llply(
				.data = resampled_data
				, .fun = function(z){
					to_return = dlply(
						.data = z
						, .variables = within
						, .fun = function(x){
							to_return = x[sample(1:nrow(x),nrow(x),replace=T),]
							return(to_return)
						}
					)
					to_return = do.call(rbind,to_return)
					return(to_return)
				}
				#, .progress = 'time'
			)
		}
	}
	resampled_data = do.call(rbind,resampled_data)
	resampled_data[,names(resampled_data)==as.character(wid)] = factor(resampled_data[,names(resampled_data)==as.character(wid)])
	return(resampled_data)
}
