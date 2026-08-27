rm(list=ls())

options(pillar.sigfig = 6)
options(contrasts = c("contr.sum", "contr.poly"))

####
shiRIf <- function(test,y,n) { if (test) {return(y)} else {return(n)} }

####
shiRFormula <- function(y,x) {
	pt1 <- paste(y,"~")
	pt2 <- paste(x,collapse="+")
	return(as.formula(paste(pt1,pt2)))
}

####
shiRDescriptive <- function(df, GrpVar=NULL) {
	FUNC_LIST <- list(
		"N"    = function(x)sum(!is.na(x)),
		"MEAN" = function(x)mean(x,na.rm=T),
		"SD"   = function(x)sd(x,na.rm=T),
		"SE"   = function(x)(sd(x,na.rm=T))/sqrt(sum(!is.na(x))),
		"MED"  = function(x)quantile(x,probs=0.50,na.rm=T),
		"Q25"  = function(x)quantile(x,probs=0.25,na.rm=T),
		"Q75"  = function(x)quantile(x,probs=0.70,na.rm=T),
		"MIN"  = function(x)min(x,na.rm=T),
		"MAX"  = function(x)max(x,na.rm=T),
		"SUM"  = function(x)sum(x,na.rm=T)
	)
	if (length(GrpVar)==0) {
		df %<>% lapply( shiRFact2Num ) %>% tibble::as.tibble()
		df %<>% dplyr::select(-c(names(df)[sapply(df,function(x)all(is.na(x)))]))
		Descr <- data.frame(Var=names(df))
		for (i in 1:length(FUNC_LIST)) {
			Descr[[names(FUNC_LIST)[i]]]<- sapply(df,FUNC_LIST[[i]])
		}
	} else {
		BY_LIST <- df %>% dplyr::select(!!sym(GrpVar))
		df %<>% dplyr::select(-!!sym(GrpVar)) %>% lapply( shiRFact2Num ) %>% tibble::as.tibble()
		df %<>% dplyr::select(-c(names(df)[sapply(df,function(x)all(is.na(x)))]))
		Descr <- aggregate(x=df, by=BY_LIST, FUN=FUNC_LIST[[1]]) %>% reshape2::melt(id.vars=GrpVar, value.name=names(FUNC_LIST)[1])
		for (i in 2:length(FUNC_LIST)) {
			Descr %<>% cbind( aggregate(x=df, by=BY_LIST, FUN=FUNC_LIST[[i]]) %>% reshape2::melt(id.vars=GrpVar, value.name=names(FUNC_LIST)[i]) %>% dplyr::select(-c(GrpVar,"variable")) )
		}
		Descr %<>% dplyr::rename("Var" = "variable")
	}
	return(Descr)
}

####
shiRFact2Num <- function(xFact) {
	if (is.factor(xFact))  {
		if (lapply(levels(xFact),is.character) %>% unlist %>% any) {
			xFact <- as.numeric(xFact)
		} else {
			xFact <- as.numeric(as.character(xFact))
		}
	}
	return(xFact)
}

####
shiRFormatPval <- function(pval,sep="") {
	pstring <- rep("",length(pval))
	for (i in 1:length(pval)) {
		pstring[i] <- ifelse(
			pval[i]>=0.995,paste(">","0.99",sep=sep),ifelse(
				pval[i]>=0.0995,paste("=",format(round(pval[i], 2), nsmall = 2),sep=sep),ifelse(
					pval[i]>=0.001,paste("=",format(round(pval[i], 3), nsmall = 3),sep=sep),paste("<","0.001",sep=sep)
				)))
	}
	return(pstring)
}

####
shiRDescriptivePrint <- function(
		df, # data frame
		GrpVar = NULL, # which group variable(s)
		Ignore = NULL, # which variables to be ignored (e.g. subject IDs)
		DoTest = T, # whether to run group comparisons (always false unless exactly 2 groups and NumTest and CatTest are correctly specified)
		NumTest = "wilcox.test", # or t.test
		CatTest = "fisher.test", # or chisq.test
		digits = 2 # for rounding purpose, must be non-negative integer, may be Inf for no rounding
) {
	
	sym=dplyr::sym
	
	if (digits == Inf) {
		ROUND <- function(num)sprintf("%g",num)
	} else {
		ROUND <- function(num)sprintf(paste0("%.",digits,"f"),num)
	}
	
	GROUP = ".shiRDescriptivePrint.Grp"
	
	if (length(GrpVar)==0) {
		df %<>% dplyr::mutate(!!sym(GROUP):="All")
		df[[GROUP]] %<>% factor()
	} else if (length(GrpVar)==1) {
		df %<>% dplyr::mutate(!!sym(GROUP):=!!sym(GrpVar[1]))
	} else {
		df %<>% dplyr::mutate(!!sym(GROUP):=!!sym(GrpVar[1]))
		for (i in 2:length(GrpVar)) {
			df[[GROUP]] %<>% interaction(df[[GrpVar[i]]])
		}
	}
	
	df[[GROUP]] <- droplevels(df[[GROUP]])
	NGRP <- nlevels(df[[GROUP]])
	GRPLEV <- levels(df[[GROUP]])
	
	DOTEST <- DoTest && NGRP == 2
	
	VarList <- names(df) %>% setdiff(c(GrpVar,GROUP,Ignore))
	
	for (i in 1:length(VarList)) {
		if (!is.numeric(df[[VarList[i]]])) {
			df[[VarList[i]]] %<>% factor()
		}
	}
	ISFACTOR <- sapply(df %>% dplyr::select(VarList),is.factor)
	
	# header
	
	N <- dplyr::count(df,!!sym(GROUP))$n
	TABLE <- "Variable"
	for (grp in 1:NGRP) {
		xTAB <- GRPLEV[grp] %>% paste0(" (n=",N[grp],")")
		TABLE %<>% c(xTAB)
	}
	
	# each variable
	for (i in 1:length(VarList)) {
		xdf <- df %>% dplyr::filter(!is.na(!!sym(VarList[i])))
		# variable name
		xTAB <- c(VarList[i], rep("",NGRP))
		TABLE %<>% rbind(xTAB)
		# no. of subjects
		xxtab <- table(xdf[[GROUP]])
		xN <- as.integer(xxtab[levels(xdf[[GROUP]])])
		xN[is.na(xN)] <- 0
		if (max(abs(N-xN))>0) {	TABLE %<>% rbind(c("No. of participants", xN)) }
		# stats for factor
		if (ISFACTOR[i]) {
			# column 0
			xTAB <- paste0(levels(df[[VarList[i]]]),", No. (%)")
			# column for each group
			for (grp in 1:NGRP) {
				xTAB1 <- dplyr::count(xdf %>% dplyr::filter(!!sym(GROUP)==GRPLEV[grp]), !!sym(VarList[i]), .drop=F)$n
				xTAB1 %<>% paste0(" (",(xTAB1/xN[grp]*100) %>% ROUND,"%)")
				xTAB %<>% cbind(xTAB1)
			}
			# stats for numerical
		} else {
			# column 0
			xTAB <- c("Mean (SD)","Median (IQR)")
			# column for each group
			for (grp in 1:NGRP) {
				xx <- xdf[[VarList[i]]][xdf[[GROUP]]==GRPLEV[grp]]
				xTAB1 <- c(mean(xx) %>% ROUND, median(xx) %>% ROUND)
				xTAB2 <- c(sd(xx) %>% ROUND,paste0(quantile(xx,.25) %>% ROUND,"–",quantile(xx,.75) %>% ROUND))
				xTAB1 %<>% paste0(" (", xTAB2, ")")
				xTAB %<>% cbind(xTAB1)
			}
		}
		# add to main table
		TABLE %<>% rbind(xTAB)
	}
	
	rownames(TABLE) <- NULL
	colnames(TABLE) <- NULL
	
	if ((tolower(NumTest) != "t.test" && tolower(NumTest) != "wilcox.test") || (tolower(CatTest) != "chisq.test" && tolower(CatTest) != "fisher.test")) {
		DOTEST = F
	}
	if (!DOTEST) {
		TABLE0 <- TABLE
		TABLE0[!sapply(TABLE0[,2],function(x)nchar(x)==0),1] %<>% {paste0("  ",.)}
		TABLE0[1,1] <- "Variable"
		print.table(TABLE0)
		return(TABLE)
	}
	
	if (tolower(NumTest) == "t.test") {
		NUMTEST = function(x,y)t.test(x,y)
	} else if (tolower(NumTest) == "wilcox.test") {
		NUMTEST = function(x,y)wilcox.test(x,y)
	}
	
	if (tolower(CatTest) == "chisq.test") {
		CATTEST = function(x,y)chisq.test(x,y)
	} else if (tolower(CatTest) == "fisher.test") {
		CATTEST = function(x,y)fisher.test(x,y)
	}
	
	xTAB <- "p-value"
	xCNT <- 0
	for (i in 2:nrow(TABLE)) {
		if (TABLE[i,2] != "") {xTAB %<>% c("")} else {
			xCNT <- xCNT+1
			xVAR <- TABLE[i,1]
			if (ISFACTOR[xCNT]) {
				xSTAT <- tryCatch(CATTEST(df[[GROUP]],df[[xVAR]])$p.value,error=function(e)NA)
			} else {
				xSTAT <- tryCatch(NUMTEST(df[[xVAR]][df[[GROUP]]==GRPLEV[1]],df[[xVAR]][df[[GROUP]]==GRPLEV[2]])$p.value,error=function(e)NA)
			}
			xTAB %<>% c(paste0("p",shiRFormatPval(xSTAT)))
		}
	}
	
	TABLE %<>% cbind(xTAB)
	
	rownames(TABLE) <- NULL
	colnames(TABLE) <- NULL
	
	TABLE0 <- TABLE
	TABLE0[!sapply(TABLE0[,2],function(x)nchar(x)==0),1] %<>% {paste0("  ",.)}
	TABLE0[1,1] <- "Variable"
	print.table(TABLE0)
	return(TABLE)
	
}



#### func def ----

####
func_ThresDens <- function(mat, density) {
	
	vals <- mat[upper.tri(mat)]
	nKeep <- round(length(vals) * density)
	cutoff <- sort(vals, T)[nKeep]
	mat[mat < cutoff] <- 0
	
	density_actual <- mean(mat[lower.tri(mat, diag=F)] != 0, na.rm=T)
	
	return(list(
		mat = mat,
		density_actual = density_actual
	))
	
}

####
func_LouvainRep <- function(mat, weighted = NULL, nRun = 100) {
	
	stopifnot(!is.null(weighted))
	
	g <- igraph::graph_from_adjacency_matrix(mat, mode="undirected", diag=F, weighted=weighted)
	wei <- shiRIf(weighted, igraph::E(g)$weight, NULL)
	
	PartAll <- list()
	QAll <- c()
	
	for (i in 1:nRun) {
		clu <- igraph::cluster_louvain(g, weights=wei)
		PartAll[[i]] <- igraph::membership(clu)
		QAll[i] <- igraph::modularity(g, membership(clu), weights=wei)
	}
	
	best <- which.max(QAll)
	
	Alleg <- matrix(0, nrow(mat), nrow(mat))
	for (i in 1:nRun) {
		part <- PartAll[[i]]
		Alleg <- Alleg + outer(part, part, "==")
	}
	Alleg <- Alleg / nRun
	
	return(list(
		PartAll = PartAll,
		PartBest = PartAll[[best]],
		QAll = QAll,
		QBest = QAll[best],
		QMean = mean(QAll),
		Alleg = Alleg
	))
	
}

####
func_Efficiency <- function(mat, weighted = NULL) {
	
	stopifnot(!is.null(weighted))
	
	g <- igraph::graph_from_adjacency_matrix(mat, mode="undirected", diag=F, weighted=weighted)
	
	if (weighted) {
		d <- 1 / igraph::E(g)$weight
		glob_eff <- igraph::global_efficiency(g, weights=d, directed=F)
	} else {
		glob_eff <- igraph::global_efficiency(g, directed=F)
	}
	
	return(glob_eff)
	
}


