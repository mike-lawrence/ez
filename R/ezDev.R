ezDev <- function(){
	if(!require(RCurl)){
		stop('Please install the "RCurl" package.')
	}
	temp = getURL('https://raw.github.com/mike-lawrence/ez/master/DESCRIPTION')
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
			stop(paste('Please install the "',this_package,'" package.',sep=''))
		}
	}
	temp = getURL('https://github.com/mike-lawrence/ez/tree/master/R')
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
			expr = parse(text=getURL(this_file))
			, envir = .GlobalEnv
		)
	}
}