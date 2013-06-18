ezANOVA_levene <-
function (y) {
	form <- y
	mf <- model.frame(form)
	if (any(sapply(2:dim(mf)[2], function(j) is.numeric(mf[[j]])))) stop("Levene's test is not appropriate with quantitative explanatory variables.")
	y <- mf[,1]
	if(dim(mf)[2]==2) {
		group <- mf[,2]
	}else {
		if (length(grep("\\+ | \\| | \\^ | \\:",form))>0) stop("Model must be completely crossed formula only.")
		group <- interaction(mf[,2:dim(mf)[2]])
	}
	if (!is.numeric(y)) 
		stop(deparse(substitute(y)), " is not a numeric variable")
	if (!is.factor(group)) {
		warning(deparse(substitute(group)), " coerced to factor.")
		group <- as.factor(group)
	}
	meds <- tapply(y, group, median, na.rm = TRUE)
	resp <- abs(y - meds[group])
	table <- as.data.frame(anova(lm(resp ~ group))[, c(1,2, 4, 5)])
	to_return = data.frame(table$D[1],table$D[2],table$S[1],table$S[2],table$F[1],table$P[1])
	names(to_return)=c("DFn", "DFd", "SSn", "SSd", "F", "p")
	to_return$"p<.05"=ifelse(to_return$p<.05,'*','')
	return(to_return)
}

ezANOVA_summary <-
function(object){
	to_return=list()
	GG <- function(SSPE, P){
		p <- nrow(SSPE)
		if (p < 2) return(NA) 
		lambda <- eigen(SSPE %*% solve(t(P) %*% P))$values
		lambda <- lambda[lambda > 0]
		((sum(lambda)/p)^2)/(sum(lambda^2)/p)
	}
	HF <- function(gg, error.df, p){
		((error.df + 1)*p*gg - 2)/(p*(error.df - p*gg))
	}
	mauchly <- function (SSD, P, df) {
		# most of this function borrowed from stats:::mauchly.test.SSD
		if (nrow(SSD) < 2) return(c(NA, NA))
		Tr <- function (X) sum(diag(X))
		p <- nrow(P)
		I <- diag(p)
		Psi <- t(P) %*% I %*% P 
		B <- SSD 
		pp <- nrow(SSD) 
		U <- solve(Psi, B)
		n <- df 
		logW <- log(det(U)) - pp * log(Tr(U/pp))
		rho <- 1 - (2 * pp^2 + pp + 2)/(6 * pp * n)
		w2 <- (pp + 2) * (pp - 1) * (pp - 2) * (2 * pp^3 + 6 * pp^2 + 
				3 * p + 2)/(288 * (n * pp * rho)^2)
		z <- -n * rho * logW
		f <- pp * (pp + 1)/2 - 1
		Pr1 <- pchisq(z, f, lower.tail = FALSE)
		Pr2 <- pchisq(z, f + 4, lower.tail = FALSE)
		pval <- Pr1 + w2 * (Pr2 - Pr1)
		c(statistic = c(W = exp(logW)), p.value = pval)
	}		
	test.statistic <- 1:4
	nterms <- length(object$terms)
	error.df <- object$error.df
	table <- data.frame(matrix(0, nterms, 8))
	table2 <- data.frame(matrix(0, nterms, 7))
	table3 <- data.frame(matrix(0, nterms, 4))
	table3[,1] <- table2[,1] <- table[,1] <- object$terms
	colnames(table) <- c("Effect","DFn", "DFd", "SSn", "SSd", "F", "p", "p<.05")
	colnames(table2) <- c("Effect","GGe", "p[GG]", "p[GG]<.05", "HFe", "p[HF]","p[HF]<.05")
	colnames(table3) <- c("Effect","W", "p", "p<.05")
	for (term in 1:nterms){
		SSP <- object$SSP[[term]]
		SSPE <- object$SSPE[[term]]
		P <- object$P[[term]]
		p <- ncol(P)
		PtPinv <- solve(t(P) %*% P)
		gg <- GG(SSPE, P)
		table[term, "SSn"] <- sum(diag(SSP %*% PtPinv))
		table[term, "SSd"] <- sum(diag(SSPE %*% PtPinv))
		table[term, "DFn"] <- object$df[term] * p
		table[term, "DFd"] <- error.df * p
		table[term, "F"] <-  (table[term, "SSn"]/table[term, "DFn"])/
			(table[term, "SSd"]/table[term, "DFd"])
		table[term, "p"] <- pf(table[term, "F"], table[term, "DFn"],
			table[term, "DFd"], lower.tail=FALSE)
		table[term, "p<.05"] = ifelse(table[term, "p"]<.05,'*','')
		table2[term, "GGe"] <- gg
		table2[term, "HFe"] <- HF(gg, error.df, p)
		table3[term,2:3] <- mauchly(SSPE, P, object$error.df)
		table3[term, "p<.05"] = ifelse(table3[term, "p"]<.05,'*','')		
	}
	ANOVA = as.data.frame(table)
	to_return$ANOVA=ANOVA
	table3=table3[!is.na(table3$W),]
	if (nrow(table3) > 0){
		to_return$'Mauchly\'s Test for Sphericity'=table3
		table2[,"p[GG]"] <- pf(table[,"F"], table2[,"GGe"]*table[,"DFn"],table2[,"GGe"]*table[,"DFd"], lower.tail=FALSE)
		table2[, "p[GG]<.05"] = ifelse(table2[, "p[GG]"]<.05,'*','')
		table2[,"p[HF]"] <- pf(table[,"F"], pmin(1, table2[,"HFe"])*table[,"DFn"],	pmin(1, table2[,"HFe"])*table[,"DFd"], lower.tail=FALSE)
		table2[, "p[HF]<.05"] = ifelse(table2[, "p[HF]"]<.05,'*','')
		table2=table2[!is.na(table2$GG),]
		to_return$'Sphericity Corrections'=table2
	}
	return(to_return)
}

ezANOVA_between_summary <-
function(from_Anova,white.adjust,between_numeric){
	temp = as.data.frame(from_Anova)
	if(!white.adjust){
		names(temp) = c('SSn','DFn','F','p')
		temp$DFd = temp$D[length(temp$D)]
		temp$SSd = temp$S[length(temp$S)]
		temp$Effect = row.names(temp)
		row.names(temp) = 1:length(temp[,1])
		temp = temp[1:(length(temp[,1])-1),c(7,2,5,1,6,3,4)]
	}else{
		names(temp) = c('DFn','F','p')
		temp$DFd = temp$D[length(temp$D)]
		temp$SSd = temp$S[length(temp$S)]
		temp$Effect = row.names(temp)
		row.names(temp) = 1:length(temp[,1])
		temp = temp[1:(length(temp[,1])-1),c(5,1,4,2,3)]
	}
	temp$'p<.05'=ifelse(temp$p<.05,'*','')
	return(temp)
}


ezANOVA_get_wide_lm<-
function(data, dv, wid, within, between){
	to_return = list()
	if(!is.null(within)){
		for(this_within in within){
			old_levs = levels(data[,names(data)==this_within])
			new_levs = rep(NA,length=length(old_levs))
			temp = strsplit(old_levs,'_')
			for(i in 1:length(old_levs)){
				new_levs[i] = paste(temp[[i]],collapse='.')
			}
			levels(data[,names(data)==this_within]) = new_levs
		}
		wide_formula = paste(paste(wid,paste(between,collapse='+'),sep='+'),paste(within,collapse='+'),sep='~')
		wide_formula = sub('+~','~',wide_formula,fixed=T)
		wide = dcast(data, wide_formula, value.var = as.character(dv))
		to_return$idata=ldply(strsplit(names(wide)[!(names(wide) %in% c(between,wid))],'_'))
		names(to_return$idata)=within
		for(this_within in within){
			to_return$idata[,names(to_return$idata)==this_within] = factor(to_return$idata[,names(to_return$idata)==this_within])
		}
		wide_dv=data.matrix(wide[,!(names(wide) %in% c(wid,between))])
		to_return$idesign_formula = paste('~',paste(within,collapse='*'),sep='')
	}else{
		wide=data
	}
	if(is.null(between)){
		lm_formula=paste('wide_dv~1',sep='')
	}else if(is.null(within)){
		lm_formula=paste(dv,'~',paste(between,collapse='*'),sep='')
	}else{
		lm_formula=paste('wide_dv~',paste(between,collapse='*'),sep='')
	}
	op <- options( "contrasts" = c( "contr.sum", "contr.poly" ) )
	to_return$lm = lm(eval(parse(text=lm_formula)),wide)
	options(op)
	return(to_return)
}

ezANOVA_main <-
function(data, dv, wid, within, within_full, within_covariates, between, between_covariates, observed, diff, reverse_diff, type, return_aov, white.adjust){
	vars = as.character(c(dv,wid,between,within,diff,within_full))
	for(var in vars){
		if(!(var %in% names(data))){
			stop(paste('"',var,'" is not a variable in the data frame provided.',sep=''))			
		}
	}
	if(is.null(within) & is.null(between)){
		stop('is.null(within) & is.null(between)\nYou must specify at least one independent variable.')
	}
	if(!is.data.frame(data)){
		stop('"data" must be a data frame.')
	}
	if(!is.numeric(data[,names(data)==dv])){
		stop('"dv" must be numeric.')
	}
	if(!is.factor(data[,names(data)==wid])){
		warning(paste('Converting "',wid,'" to factor for ANOVA.',sep=''),immediate.=TRUE,call.=FALSE)
		data[,names(data)==wid]=factor(data[,names(data)==wid])
	}else{
		if(length(unique(data[,names(data)==wid]))!=length(levels(data[,names(data)==wid]))){
			warning(paste('You have removed one or more Ss from the analysis. Refactoring "',wid,'" for ANOVA.',sep=''),immediate.=TRUE,call.=FALSE)
			data[,names(data)==wid]=factor(data[,names(data)==wid])
		}
	}
	within_numeric = rep(FALSE,length(within))
	if(!is.null(within)){
		for(i in 1:length(within)){
			if(is.numeric(data[,names(data)==within[i]])){
					warning(paste('"',within[i],'" will be treated as numeric.',sep=''),immediate.=TRUE,call.=FALSE)
					within_numeric[i] = TRUE
			}else{
				if(!is.factor(data[,names(data)==within[i]])){
					warning(paste('Converting "',within[i],'" to factor for ANOVA.',sep=''),immediate.=TRUE,call.=FALSE)
					data[,names(data)==within[i]]=factor(data[,names(data)==within[i]])
				}
				if(length(unique(data[,names(data)==within[i]]))!=length(levels(data[,names(data)==within[i]]))){
					warning(paste('You have removed one or more levels from variable "',within[i],'". Refactoring for ANOVA.',sep=''),immediate.=TRUE,call.=FALSE)
					data[,names(data)==within[i]]=factor(data[,names(data)==within[i]])
				}
				if(length(levels(data[,names(data)==within[i]]))==1){
					stop(paste('"',within[i],'" has only one level."',sep=''))			
				}	
			}
		}
	}
	between_numeric = rep(FALSE,length(between))
	if(!is.null(between)){
		for(i in 1:length(between)){
			if(is.numeric(data[,names(data)==between[i]])){
				warning(paste('"',between[i],'" will be treated as numeric.',sep=''),immediate.=TRUE,call.=FALSE)
				between_numeric[i] = TRUE
			}else{
				if(!is.factor(data[,names(data)==between[i]])){
					warning(paste('Converting "',between[i],'" to factor for ANOVA.',sep=''),immediate.=TRUE,call.=FALSE)
					data[,names(data)==between[i]]=factor(data[,names(data)==between[i]])
				}
				if(length(unique(data[,names(data)==between[i]]))!=length(levels(data[,names(data)==between[i]]))){
					warning(paste('You have removed one or more levels from variable "',between[i],'". Refactoring for ANOVA.',sep=''),immediate.=TRUE,call.=FALSE)
					data[,names(data)==between[i]]=factor(data[,names(data)==between[i]])
				}
				if(length(levels(data[,names(data)==between[i]]))==1){
					stop(paste('"',between[i],'" has only one level."',sep=''))			
				}	
			}
		}
		temp = ddply(
			idata.frame(data)
			,structure(as.list(c(wid,between)),class = 'quoted')
			,function(x){
				to_return = 0
				return(to_return)
			}
		)
		wid_temp = data.frame(table(temp[,names(temp)==wid]))
		if(any(wid_temp$Freq>1)){
			warning(paste('The column supplied as the wid variable contains non-unique values across levels of the supplied between-Ss variables. Automatically fixing this by generating unique wid labels.',sep=''),immediate.=TRUE,call.=FALSE)
			data[,names(data)==wid] = as.character(data[,names(data)==wid])
			for(i in unique(as.character(between))){
				data[,names(data)==wid] = paste(data[,names(data)==wid],data[,names(data)==i])
			}
			data[,names(data)==wid] = factor(data[,names(data)==wid])
			temp = ddply(
				idata.frame(data)
				,structure(as.list(c(wid,between)),class = 'quoted')
				,function(x){
					to_return = 0
					return(to_return)
				}
			)
		}
		temp = ddply(
			temp
			,structure(as.list(c(between)),class = 'quoted')
			,function(x){
				to_return = data.frame(
					N = nrow(x)
				)
				return(to_return)
			}
		)
		balanced = ifelse(
			length(unique(temp$N))>1
			, FALSE
			, TRUE
		)
		if(!balanced){
			warning('Data is unbalanced (unequal N per group). Make sure you specified a well-considered value for the type argument to ezANOVA().',immediate.=TRUE,call.=FALSE)
		}
	}
	if(!is.null(diff)){
		if(length(diff)>1){
			stop(paste('Provide only one value to "diff".',sep=''))
		}
		if(!is.factor(data[,names(data)==diff])){
			warning(paste('Converting "',diff,'" to factor for computing difference score.',sep=''),immediate.=TRUE,call.=FALSE)
			data[,names(data)==diff]=factor(data[,names(data)==diff])
		}
		temp <- ddply(
			idata.frame(data)
			,structure(as.list(c(wid,diff)),class = 'quoted')
			,function(x){
				to_return = 0
				return(to_return)
			}
		)
		if(!all(as.data.frame(table(temp[,names(temp) %in% c(wid,within)]))$Freq==2)){
			stop(paste('Variable supplied to "diff" ("',as.character(diff),'") does not appear to be a within variable.',sep=''))
		}
		data[,names(data)==as.character(diff)] = factor(data[,names(data)==as.character(diff)])
		if(length(unique(data[,names(data)==as.character(diff)]))!=2){
			stop('The column passed as argument "diff" must have precisely 2 levels.')
		}
		if(reverse_diff){
			data[,names(data)==as.character(diff)] = factor(data[,names(data)==as.character(diff)],levels=rev(levels(data[,names(data)==as.character(diff)])))
		}
	}
	########
	# computing residuals from covariates
	########
	if((!is.null(between_covariates))|(!is.null(within_covariates))){
		warning("Implementation of ANCOVA in this version of ez is experimental and not yet fully validated. Also, note that ANCOVA is intended purely as a tool to increase statistical power; ANCOVA can not eliminate confounds in the data. Specifically, covariates should: (1) be uncorrelated with other predictors and (2) should have effects on the DV that are independent of other predictors. Failure to meet these conditions may dramatically increase the rate of false-positives.",immediate.=TRUE,call.=FALSE)
	}
	if(!is.null(between_covariates)){
		temp = idata.frame(cbind(data,ezDV=data[,names(data) == as.character(dv)]))
		temp <- ddply(
			temp
			,structure(as.list(c(wid,between_covariates,within,within_full,diff)),class = 'quoted')
			,function(x){
				to_return = mean(x$ezDV)
				names(to_return) = as.character(dv)
				return(to_return)
			}
		)
		temp = idata.frame(cbind(temp,ezDV=temp[,names(temp) == as.character(dv)]))
		temp <- ddply(
			temp
			,structure(as.list(c(wid,between_covariates)),class = 'quoted')
			,function(x){
				to_return = mean(x$ezDV)
				names(to_return) = as.character(dv)
				return(to_return)
			}
		)
		for(cov in between_covariates){
			temp$ezCov = temp[,names(temp)==cov]
			if(is.factor(temp$ezCov)){
				contrasts(temp$ezCov) = 'contr.helmert'
			}else{
				temp$ezCov = temp$ezCov - mean(temp$ezCov)
				warning('Covariate"',cov,'" is numeric and will therefore be fit to a linear effect.',immediate.=TRUE,call.=FALSE)
			}
			fit = eval(parse(text=paste('lm(formula=',dv,'~ezCov,data=temp)')))
			temp$fitted = fitted(fit)
			for(cov_lev in as.character(unique(temp[,names(temp)==cov]))){
				data[as.character(data[,names(data)==cov])==cov_lev,names(data)==dv] = data[as.character(data[,names(data)==cov])==cov_lev,names(data)==dv] - temp$fitted[as.character(temp[,names(temp)==cov])==cov_lev][1] + as.numeric(coef(fit)[1])
			}
		}
	}
	if(!is.null(within_covariates)){
		temp = idata.frame(cbind(data,ezDV=data[,names(data) == as.character(dv)]))
		temp <- ddply(
			temp
			,structure(as.list(c(wid,within_covariates,within,within_full,diff)),class = 'quoted')
			,function(x){
				to_return = mean(x$ezDV)
				names(to_return) = as.character(dv)
				return(to_return)
			}
		)
		for(cov in as.character(within_covariates)){
			temp2 <- ddply(
				.data = temp
				, .variables = eval(parse(text=paste('.(',wid,',',cov,')')))#structure(as.list(c(wid,as.symbol(cov)))),class = 'quoted')
				, .fun = function(x){
					to_return = data.frame(
						value = mean(x[,names(x)==dv])
					)
					names(to_return) = as.character(dv)
					return(to_return)
				}
			)
			if(is.numeric(temp2[,names(temp2)==cov])){
				warning('Covariate"',cov,'" is numeric and will therefore be fit to a linear effect.',immediate.=TRUE,call.=FALSE)
			}
			for(this_wid in unique(as.character(data[,names(data)==wid]))){
				temp3 = temp2[temp2[,names(temp2)==wid]==this_wid,]
				temp3$ezCov = temp3[,names(temp3)==cov]
				if(is.factor(temp3$ezCov)){
					contrasts(temp3$ezCov) = 'contr.helmert'
				}else{
					temp3$ezCov = temp3$ezCov - mean(temp3$ezCov)
				}
				fit = eval(parse(text=paste('lm(formula=',dv,'~ezCov,data=temp3)')))
				temp3$fitted = fitted(fit)
				for(cov_lev in unique(as.character(temp3[,names(temp3)==cov]))){
					data[(as.character(data[,names(data)==cov])==cov_lev)&(data[,names(data)==wid]==this_wid),names(data)==dv] = data[(as.character(data[,names(data)==cov])==cov_lev)&(data[,names(data)==wid]==this_wid),names(data)==dv] - temp3$fitted[as.character(temp3[,names(temp3)==cov])==cov_lev][1] + as.numeric(coef(fit)[1])
				}
			}
		}
	}
	########
	# Collapsing the data to cell means
	########
	if(!is.null(within_full)){
		warning(paste('Collapsing data to cell means first using variables supplied to "within_full", then collapsing the resulting means to means for the cells supplied to "within".',sep=''),immediate.=TRUE,call.=FALSE)
		temp = idata.frame(cbind(data,ezDV=data[,names(data) == as.character(dv)]))
		data <- ddply(
			temp
			,structure(as.list(c(wid,between,within,within_full,diff)),class = 'quoted')
			,function(x){
				to_return = mean(x$ezDV)
				names(to_return) = as.character(dv)
				return(to_return)
			}
		)
		temp = idata.frame(cbind(data,ezDV=data[,names(data) == as.character(dv)]))
		data <- ddply(
			temp
			,structure(as.list(c(wid,between,within,diff)),class = 'quoted')
			,function(x){
				to_return = mean(x$ezDV)
				names(to_return) = as.character(dv)
				return(to_return)
			}
		)
	}else{
		data <- ddply(
			data
			,structure(as.list(c(wid,between,within,diff)),class = 'quoted')
			,function(x){
				to_return = data.frame(
					temp = mean(x[,names(x)==as.character(dv)])
					, ezNum = nrow(x)
				)
				names(to_return)[1] = as.character(dv)
				return(to_return)
			}
		)
		if(any(data$ezNum>1)){
			warning(paste('Collapsing data to cell means. *IF* the requested effects are a subset of the full design, you must use the "within_full" argument, else results may be inaccurate.',sep=''),immediate.=TRUE,call.=FALSE)
		}
	}
	if(any(is.na(data[,names(data)==as.character(dv)]))){
		stop('One or more cells returned NA when aggregated to a mean. Check your data.')
	}
	if(is.null(diff)){
		if(!all(as.data.frame(table(data[,names(data) %in% c(wid,within)]))$Freq==1)){
			stop('One or more cells is missing data. Try using ezDesign() to check your data.')
		}
	}else{
		if(!all(as.data.frame(table(data[,names(data) %in% c(wid,within,diff)]))$Freq==1)){
			stop('One or more cells is missing data. Try using ezDesign() to check your data.')
		}		
	}
	if(!is.null(between)){
		if(any(as.data.frame(table(data[,names(data) %in% c(between)]))$Freq==0)){
			stop('One or more cells is missing data. Try using ezDesign() to check your data.')
		}
	}
	if(!is.null(diff)){
		warning(paste('Collapsing "',as.character(diff),'" to a difference score ("',levels(data[,names(data)==as.character(diff)])[2],'"-"',levels(data[,names(data)==as.character(diff)])[1],'") prior to computing statistics.',sep=''),immediate.=TRUE,call.=FALSE)
		temp = idata.frame(cbind(data,ezDV=data[,names(data) == as.character(dv)]))
		data <- ddply(
			temp
			,structure(as.list(c(wid,within,between)),class = 'quoted')
			,function(x){
				to_return = diff(x$ezDV)
				names(to_return) = as.character(dv)
				return(to_return)
			}
		)
		temp = names(within)
		temp = temp[!(within %in% diff)]
		within = within[!(within %in% diff)]
		names(within) = temp
	}
	to_return = list()
	if(is.null(between)){
		if(any(within_numeric)){
			warning('There is at least one numeric within variable, therefore aov() will be used for computation and no assumption checks will be obtained.',immediate.=TRUE,call.=FALSE)
			from_aov = ezANOVA_aov(data, dv, wid, within, between)
			to_return$ANOVA = from_aov$ANOVA
			if(return_aov){
				to_return$aov = from_aov$aov
			}
		}else{ #there are no numeric within-Ss variables
			if(type!=1){
				wide_lm = ezANOVA_get_wide_lm(data, dv, wid, within, between)
				to_return = NULL
				try(
					to_return <- ezANOVA_summary(
						Anova(
							wide_lm$lm
							, idata = wide_lm$idata
							, type = 3 #hardcoded to 3, else car::Anova() prints a note
							, idesign = eval(parse(text=wide_lm$idesign_formula))
						)
					)
				)
				if(is.null(to_return)){
					stop('The car::Anova() function used to compute results and assumption tests seems to have failed. Most commonly this is because you have too few subjects relative to the number of cells in the within-Ss design. It is possible that trying the ANOVA again with "type=1" may yield results (but definitely no assumption tests).')
				}
				if(return_aov){
					from_aov = ezANOVA_aov(data, dv, wid, within, between)
					to_return$aov = from_aov$aov
				}
			}else{
				from_aov = ezANOVA_aov(data, dv, wid, within, between)
				to_return$ANOVA = from_aov$ANOVA
				if(return_aov){
					to_return$aov = from_aov$aov
				}
			}
		}
	}else{ #there are between-Ss variables
		if(balanced){
			if(is.null(within)){
				wide_lm = ezANOVA_get_wide_lm(data, dv, wid, within, between)
				from_Anova = Anova(
					wide_lm$lm
					, type = type
					, white.adjust = white.adjust
				)
				to_return$ANOVA = ezANOVA_between_summary(from_Anova,white.adjust,between_numeric)
				if(!any(between_numeric)){
					to_return$'Levene\'s Test for Homogeneity of Variance' = ezANOVA_levene(wide_lm$lm)
				}else{
					warning('At least one numeric between-Ss variable detected, therefore no assumption test will be returned.',immediate.=TRUE,call.=FALSE)
				}
				if(return_aov){
					from_aov = ezANOVA_aov(data, dv, wid, within, between)
					to_return$aov = from_aov$aov
				}
			}else{ #there are within-Ss variables
				if(any(within_numeric)){
					warning('There is at least one numeric within variable, therefore aov() will be used for computation and no assumption checks will be obtained.',immediate.=TRUE,call.=FALSE)
					from_aov = ezANOVA_aov(data, dv, wid, within, between)
					to_return$ANOVA = from_aov$ANOVA
					if(return_aov){
						to_return$aov = from_aov$aov
					}
				}else{ #there are no numeric within-Ss variables
					if(type!=1){
						wide_lm = ezANOVA_get_wide_lm(data, dv, wid, within, between)
						to_return = NULL
						try(
							to_return <- ezANOVA_summary(
								Anova(
									wide_lm$lm
									, idata = wide_lm$idata
									, type = type
									, idesign = eval(parse(text=wide_lm$idesign_formula))
								)
							)
						)
						if(is.null(to_return)){
							stop('The car::Anova() function used to compute results and assumption tests seems to have failed. Most commonly this is because you have too few subjects relative to the number of cells in the within-Ss design. It is possible that trying the ANOVA again with "type=1" may yield results (but definitely no assumption tests).')
						}
						if(return_aov){
							from_aov = ezANOVA_aov(data, dv, wid, within, between)
							to_return$aov = from_aov$aov
						}
					}else{ #type 1 was requested
						from_aov = ezANOVA_aov(data, dv, wid, within, between)
						to_return$ANOVA = from_aov$ANOVA
						if(return_aov){
							to_return$aov = from_aov$aov
						}
					}
				}
			}
		}else{ #data is imbalanced
			if(type==1){
				if((length(between)>1)|(!is.null(within))){
					warning('Using "type==1" is highly questionable when data are unbalanced and there is more than one variable. Hopefully you are doing this for demonstration purposes only!',immediate.=TRUE,call.=FALSE)
				}
				from_aov = ezANOVA_aov(data, dv, wid, within, between)
				to_return$ANOVA = from_aov$ANOVA
				if(return_aov){
					to_return$aov = from_aov$aov
				}
			}else{
				if(is.null(within)){
					wide_lm = ezANOVA_get_wide_lm(data, dv, wid, within, between)
					from_Anova = Anova(
						wide_lm$lm
						, type = type
						, white.adjust = white.adjust
					)
					to_return$ANOVA = ezANOVA_between_summary(from_Anova,white.adjust,between_numeric)
					if(!any(between_numeric)){
						to_return$'Levene\'s Test for Homogeneity of Variance' = ezANOVA_levene(wide_lm$lm)
					}else{
						warning('At least one numeric between-Ss variable detected, therefore no assumption test will be returned.',immediate.=TRUE,call.=FALSE)
					}
					if(return_aov){
						from_aov = ezANOVA_aov(data, dv, wid, within, between)
						to_return$aov = from_aov$aov
					}
				}else{
					if(any(within_numeric)){
						stop('Cannot perform ANOVA when data are imbalanced and when one or more within-Ss variables are numeric. Try ezMixed() instead.')
					}
					wide_lm = ezANOVA_get_wide_lm(data, dv, wid, within, between)
					to_return = NULL
					try(
						to_return <- ezANOVA_summary(
							Anova(
								wide_lm$lm
								, idata = wide_lm$idata
								, type = type
								, idesign = eval(parse(text=wide_lm$idesign_formula))
							)
						)
					)
					if(is.null(to_return)){
						stop('The car::Anova() function used to compute results and assumption tests seems to have failed. Most commonly this is because you have too few subjects relative to the number of cells in the within-Ss design. It is possible that trying the ANOVA again with "type=1" may yield results (but definitely no assumption tests).')
					}
					if(return_aov){
						from_aov = ezANOVA_aov(data, dv, wid, within, between)
						to_return$aov = from_aov$aov
					}
				}
			}
		}
	}
	to_return$ANOVA = to_return$ANOVA[order(str_count(to_return$ANOVA$Effect,':')),]
	to_return$data = data
	return(to_return)
}

ezANOVA_aov <-
function(data, dv, wid, within, between){
	aov_formula = paste(
		as.character(dv)
		,'~'
		,paste(as.character(between),collapse = '*')
		,ifelse(is.null(between),'',ifelse(is.null(within),'','*'))
		,paste(as.character(within),collapse = '*')
		,ifelse(
			is.null(within)
			,''
			,paste(
				'+Error('
				,as.character(wid)
				,'/('
				,paste(as.character(within),collapse = '*')
				,'))'
				,sep = ''
			)
		)
		,sep = ''
	)	
	this_aov = aov(
		formula(aov_formula)
		,data = data
	)
	ANOVA = NULL
	for(x in summary(this_aov)){
		if(length(x)==1){
			x=x[[1]]
		}
		for(row in 1:length(x[,1])){
			if(!is.na(x$P[row])){
				ANOVA = rbind(
					ANOVA
					, data.frame(
						Effect=strsplit(row.names(x)[row],' ')[[1]][1]
						, DFn=x$D[row]
						, DFd=x$D[length(x$D)]
						, SSn=x$S[row]
						, SSd=x$S[length(x$S)]
						, F=x$F[row]
						, p=x$P[row]
					)
				)
			}
		}
	}
	ANOVA$'p<.05'=ifelse(ANOVA$p<.05,'*','')
	to_return = list(
		ANOVA = ANOVA
		, aov = this_aov
	)
	return(to_return)
}

ezPerm_aov <-
function(data, aov_formula){
	this_aov = aov(
		formula(aov_formula)
		,data = data
	)
	f_list=llply(this_aov,function(x){summary(x)[[1]]$F})
	f = NULL
	for(i in f_list){
		f=c(f,i)
	}
	f=f[!is.na(f)]
	return(f)
}

########
# Framework for ezANOVA branching:
########

#if no between
#	if any within numeric
#		use aov
#	else if there are within numeric
#		if type != 1
#			use Anova
#		else if type==1
#			use aov
#else if there are between
#	if between is balanced
#		if there are no within
#			use Anova
#			if there are no numeric between
#				report levene
#		else if there are within
#			if any within numeric
#				use aov
#			else if there are within numeric
#				if type != 1
#					use Anova
#				else if type==1
#					use aov
#	else if between is unbalanced
#		if type==1
#			if more than one variable
#				print big warning
#			use aov with big warning
#		else if type!= 1
#			if there are no within
#				use Anova
#				if there are no numeric between
#					report levene
#			else if there are within
#				if there are any within numeric
#					stop with error
#				else if there are no within numeric
#					use Anova
