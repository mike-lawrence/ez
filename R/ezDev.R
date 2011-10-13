ezDev <- function(do_installs=TRUE){
	if(!require(RCurl)){
		if(do_installs){
			cat('You must install the "RCurl" package to run ezDev(). Attempting install...')
			install.packages('RCurl')
			require(RCurl)
		}else{
			stop('Please install the "RCurl" package.')
		}
	}
	temp = getURL('https://raw.github.com/mike-lawrence/ez/master/DESCRIPTION',ssl.verifypeer=FALSE)
	temp = strsplit(temp,'\n')[[1]]
	temp = temp[grep('Depends: ',temp)]
	temp = sub('Depends: ','',temp,fixed=T)
	temp = strsplit(temp,',')[[1]]
	temp = sub('>','',temp,fixed=T)
	temp = sub('=','',temp,fixed=T)
	temp = gsub(' ','',temp,fixed=T)
	temp = gsub('-','',temp,fixed=T)
	temp = gsub('\\.','',temp,fixed=T)
	temp = gsub('\\(.*\\)','',temp)
	temp = temp[temp!='R']
	for(this_package in temp){
		if(!require(this_package,character.only=TRUE)){
			if(do_installs){
				cat(paste('You must install the "',this_package,'" package to run ezDev(). Attempting install...'),sep='')
				install.packages(this_package)
				require(eval.parse(text=this_package)))
			}else{
				stop(paste('Please install the "',this_package,'" package.',sep=''))
			}
		}
	}
	temp = getURL('https://github.com/mike-lawrence/ez/tree/master/R',ssl.verifypeer=FALSE)
	temp = strsplit(temp,'\n')[[1]]
	temp = temp[temp!='']
	temp = temp[str_detect(temp,'            <td class="content"> <a href="/mike-lawrence/ez/blob/')]
	files = str_extract(temp,'/R/.*R\\" class')
	files = str_replace(files,'/R/','')
	files = str_replace(files,'\\" class','')
	for(this_file in files){
		this_file = paste(
			'https://raw.github.com/mike-lawrence/ez/master/R/'
			, this_file
			, sep = ''
		)
		eval(
			expr = parse(text=getURL(this_file,ssl.verifypeer=FALSE))
			, envir = .GlobalEnv
		)
	}
}