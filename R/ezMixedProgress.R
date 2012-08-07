ezMixedProgress <-
function(
	progress_dir
	, return_models = TRUE
){
	to_return = list(
		summary = NULL
	)
	files = list.files(
		path = progress_dir
		, pattern = '.RData'
		, full.names = TRUE
	)
	files = files[order(str_count(files,'BY'),files)]
	for(this_file in files){
		out_from_process_term = NULL
		load(this_file)
		to_return$summary = rbind(to_return$summary,out_from_process_term$summary)
		eval(parse(text=paste('to_return$formulae$"',as.character(out_from_process_term$summary$effect),'" = out_from_process_term$formulae',sep='')))
		eval(parse(text=paste('to_return$warnings$"',as.character(out_from_process_term$summary$effect),'" = out_from_process_term$warnings',sep='')))
		eval(parse(text=paste('to_return$errors$"',as.character(out_from_process_term$summary$effect),'" = out_from_process_term$errors',sep='')))
		rm(out_from_process_term)
		gc()
	}
	if(return_models){
		effects = list.files(
			path = paste(progress_dir,'models',sep='/')
		)
		effects = effects[order(str_count(effects,'BY'),effects)]
		for(this_effect in effects){
			this_effect_name = str_replace_all(this_effect,'BY',':')
			fits = list.files(
				path = paste(progress_dir,'models',this_effect,sep='/')
			)
			for(this_fit in fits){
				load(paste(progress_dir,'models',this_effect,this_fit,sep='/'))
				this_fit_name = str_replace(this_fit,'.RData','')
				eval(parse(text=paste('to_return$models$"',this_effect_name,'"$"',this_fit_name,'" = ',this_fit_name,sep='')))
			}
		}
		
		
	}
	return(to_return)
}
