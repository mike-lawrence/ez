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
			ids = sample(as.character(unique(data[,names(data)==as.character(wid)])),replace=T)
		}
		id_list = list()
		for(i in 1:length(ids)){
			id_list[[i]] = list(num=i,this_id=ids[i])
		}
		resampled_data = ldply(
			.data = id_list
			, .fun = function(x){
				to_return = data[as.character(data[,names(data)==as.character(wid)])==x$this_id,]
				to_return[,names(to_return)==as.character(wid)] = x$num
				return(to_return)
			}
		)
		resampled_data[,names(resampled_data)==as.character(wid)] = factor(resampled_data[,names(resampled_data)==as.character(wid)])
	}else{
		resampled_data = data
	}
	if(resample_within){
		to_return = ddply(
			.data = resampled_data
			, .variables = structure(as.list(c(wid,within)),class = 'quoted')
			, .fun = function(x){
	 			to_return = x[sample(1:nrow(x),nrow(x),replace=T),]
				return(to_return)
			}
		)
	}else{
		to_return = resampled_data
	}
	return(to_return)
}
