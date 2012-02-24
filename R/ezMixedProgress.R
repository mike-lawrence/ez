ezMixedProgress <-
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
		to_return$summary = rbind(to_return$summary,out_from_process_term$summary)
		eval(parse(text=paste('to_return$formulae$"',as.character(out_from_process_term$summary$effect),'" = out_from_process_term$formulae',sep='')))
		eval(parse(text=paste('to_return$warnings$"',as.character(out_from_process_term$summary$effect),'" = out_from_process_term$warnings',sep='')))
		eval(parse(text=paste('to_return$errors$"',as.character(out_from_process_term$summary$effect),'" = out_from_process_term$errors',sep='')))
		rm(out_from_process_term)
		gc()
	}
	return(to_return)
}
