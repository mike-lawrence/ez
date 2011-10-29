get_ezMixed_info <-
function(
	progress_dir
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
		load(this_file)
		to_return$summary = rbind(to_return$summary,temp$summary)
		eval(parse(text=paste('to_return$models$"',as.character(temp$summary$effect),'" = temp$models',sep='')))
		eval(parse(text=paste('to_return$warnings$"',as.character(temp$summary$effect),'" = temp$warnings',sep='')))
		eval(parse(text=paste('to_return$errors$"',as.character(temp$summary$effect),'" = temp$errors',sep='')))
		rm(temp)
		gc()
	}
	return(to_return)
}